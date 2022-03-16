package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_018(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "Jaleco SS 88006"

  private[this] val prgBank = Array.ofDim[Int](3)
  private[this] val chrBank = Array.ofDim[Int](8)
  private[this] var irqEnabled = false
  private[this] var irqReload,irqCounter = 0
  private[this] var irqCounterMask = 0xFFFF

  override def reset: Unit =
    java.util.Arrays.fill(prgBank,0)
    java.util.Arrays.fill(chrBank,0)
    irqEnabled = false
    irqCounterMask = 0xFFFF
    irqReload = 0
    irqReload = 0

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(chrBank)
    out.writeObject(prgBank)
    out.writeBoolean(irqEnabled)
    out.writeInt(irqCounter)
    out.writeInt(irqReload)
    out.writeInt(irqCounterMask)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(chrBank,in)
    loadMemory(prgBank,in)
    irqEnabled = in.readBoolean()
    irqCounter = in.readInt()
    irqCounter = in.readInt()
    irqReload = in.readInt()
    irqCounterMask = in.readInt()
    super.loadState(in)

  override def writeCPU(address: Int, value: Int): Unit =
    address & 0xF003 match
      case 0x8000 =>
        prgBank(0) = (prgBank(0) & 0x30) | (value & 0xF)
      case 0x8001 =>
        prgBank(0) = (prgBank(0) & 0xF) | ((value & 0x03) << 4)
      case 0x8002 =>
        prgBank(1) = (prgBank(1) & 0x30) | (value & 0xF)
      case 0x8003 =>
        prgBank(1) = (prgBank(1) & 0xF) | ((value & 0x03) << 4)
      case 0x9000 =>
        prgBank(2) = (prgBank(2) & 0x30) | (value & 0xF)
      case 0x9001 =>
        prgBank(2) = (prgBank(2) & 0xF) | ((value & 0x03) << 4)
      case 0x9002 =>
        sramEnabled = (value & 1) > 0
        sramReadOnly = (value & 2) == 0
      case 0xE000 =>
        irqReload = (irqReload & 0xFFF0) | (value & 0xF)
      case 0xE001 =>
        irqReload = (irqReload & 0xFF0F) | (value & 0xF) << 4
      case 0xE002 =>
        irqReload = (irqReload & 0xF0FF) | (value & 0xF) << 8
      case 0xE003 =>
        irqReload = (irqReload & 0x0FFF) | (value & 0xF) << 12
      case 0xF000 =>
        irqCounter = irqReload
        irqHandler(false)
      case 0xF001 =>
        irqEnabled = (value & 1) > 0
        irqCounterMask = if (value & 8) > 0 then 0x000F else if (value & 4) > 0 then 0x00FF else if (value & 2) > 0 then 0x0FFF else 0xFFFF
        irqHandler(false)
      case 0xF002 =>
        import Cartridge.Mirroring.*
        value & 3 match
          case 0 => setMirroring(HORIZONTAL)
          case 1 => setMirroring(VERTICAL)
          case 2 => setMirroring(ALL_MIRROR0)
          case 3 => setMirroring(ALL_MIRROR1)
      case 0xF003 =>
        // sound not implemented
      case adr =>
        if address >= 0xA000 && address < 0xE000 then
          val index = ((adr >> 12) - 0xA) << 1 | (adr & 2) >> 1
          if (adr & 1) == 0 then
            chrBank(index) = (chrBank(index) & 0xF0) | (value & 0x0F)
          else
            chrBank(index) = (chrBank(index) & 0x0F) | (value & 0x0F) << 4
        else
          super.writeCPU(address, value)

  override def cpuClock(): Unit =
    if irqEnabled then
      if (irqCounter & irqCounterMask) == 0 then
        irqHandler(true)

      irqCounter = (irqCounter & (~irqCounterMask & 0xFFFF)) | ((irqCounter - 1) & irqCounterMask)

  override def readPRG(address: Int): Int =
    if address < 0xE000 then
      readPRG_8K(address,prgBank((address >> 13) - 4))
    else
      readPRG_8K(address,(ROM_SIZE << 1) - 1)

  override def readCHR(address: Int): Int =
    readCHR_1K(address,chrBank(address >> 10))

