package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_069(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "Sunsoft FME-7"

  private[this] var cmdReg = 0
  private[this] val chrBank = Array.ofDim[Int](8)
  private[this] val prgBank = Array.ofDim[Int](3)
  private[this] var romRamBank = 0
  private[this] var ramSelected = false
  private[this] var irqEnabled,irqCounterEnabled = false
  private[this] var irqCounter = 0

  override def readRAM(address: Int): Int =
    if ramSelected then
      super.readRAM(address)
    else
      readCHR_1K(address,romRamBank)

  override def readCHR(address: Int): Int =
    readCHR_1K(address,chrBank(address >> 10))

  override def readPRG(address: Int): Int =
    if address < 0xE000 then
      readPRG_8K(address,prgBank((address >> 13) - 4))
    else
      readPRG_8K(address,(ROM_SIZE << 1) - 1)

  override def writeCPU(address: Int, value: Int): Unit =
    if address < 0x8000 then
      // RAM or ROM
      if ramSelected then
        if sramEnabled && SRAM != null then // RAM present and enabled
          SRAM(address & 0x1FFF) = value
    else if address < 0xA000 then
      cmdReg = value & 0xF
    else if address < 0xC000 then
      handleCommand(value)

  inline private def handleCommand(value:Int):Unit =
    if cmdReg < 8 then
      chrBank(cmdReg) = value
    else cmdReg match
      case 8 =>
        romRamBank = value & 0x3F
        ramSelected = (value & 0x40) > 0
        sramEnabled = (value & 0x80) > 0
      case 9|0xA|0xB =>
        prgBank(cmdReg - 9) = value & 0x3F
      case 0xC =>
        import Cartridge.Mirroring.*
        value & 3 match
          case 0 => setMirroring(VERTICAL)
          case 1 => setMirroring(HORIZONTAL)
          case 2 => setMirroring(ALL_MIRROR0)
          case 3 => setMirroring(ALL_MIRROR1)
      case 0xD =>
        irqEnabled = (value & 1) > 0
        irqCounterEnabled = (value & 0x80) > 0
        irqHandler(false)
      case 0xE =>
        irqCounter = (irqCounter & 0xFF00) | value
      case 0xF =>
        irqCounter = (irqCounter & 0x00FF) | (value << 8)

  override def cpuClock(): Unit =
    if irqEnabled && irqCounterEnabled then
      irqCounter = (irqCounter - 1) & 0xFFFF
      if irqCounter == 0xFFFF then
        irqHandler(true)

