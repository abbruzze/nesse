package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_073(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "VRC3"

  private var irqLatch = 0
  private var irqControl = 0
  private var irqCounter = 0

  override def reset: Unit =
    super.reset
    irqLatch = 0
    irqCounter = 0
    irqControl = 0
  
  override protected def readPRG(address: Int) : Int =
    if address < 0xC000 then ROM(romBankIndex)(address & 0x3FFF) // banked
    else ROM(ROM.length - 1)(address & 0x3FFF)

  override protected def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    address & 0xF000 match
      case 0xF000 =>
        romBankIndex = value & 7
      case 8 =>
        irqLatch = (irqLatch & 0xFFF0) | (value & 0xF)
      case 9 =>
        irqLatch = (irqLatch & 0xFF0F) | (value & 0xF) << 4
      case 0xA =>
        irqLatch = (irqLatch & 0xF0FF) | (value & 0xF) << 8
      case 0xB =>
        irqLatch = (irqLatch & 0x0FFF) | (value & 0xF) << 12
      case 0xC =>
        irqControl = value & 7
        irqHandler(false)
        if ((irqControl & 2) == 2) irqCounter = irqLatch
      case 0xD =>
        irqHandler(false)
        irqControl = (irqControl & 0x5) | (irqControl & 1) << 1
      case _ =>

  override def cpuClock(): Unit =
    if ((irqControl & 2) == 2)
      irqControl & 4 match
        case 1 /* 8 bit */ =>
          val high = irqCounter & 0xFF00
          irqCounter += 1
          if ((irqCounter & 0x00FF) == 0)
            irqHandler(true)
            irqCounter = high | irqLatch & 0xFF
        case 0 /* 16 bit */ =>
          irqCounter += 1
          if (irqCounter == 0xFFFF)
            irqHandler(true)
            irqCounter = irqLatch

