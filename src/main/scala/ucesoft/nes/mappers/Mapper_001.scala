package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, Clock, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_001(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  import Cartridge.*

  override val cartName : String = "MMC1"

  private[this] var shiftRegister = 0
  private[this] var shiftRegWriteCount = 0
  private[this] var lastCPUClockWriteCycle = 0L
  private[this] val clk = Clock.systemClock // PPU clock = 3 * CPU clock
  private[this] var prgRomBankMode = 3
  private[this] var chrBankMode = 0
  private[this] var chr0BankIndex,chr1BankIndex = 0
  private[this] var ctrlReg = 0xC

  private[this] final val CONTROL_REG = 0
  private[this] final val CHR_BANK_0_REG = 1
  private[this] final val CHR_BANK_1_REG = 2
  private[this] final val PRG_BANK_REG = 3

  override def reset: Unit =
    super.reset
    ctrlReg = 0xC
    prgRomBankMode = 3
    chrBankMode = 0
    chr0BankIndex = 0
    chr1BankIndex = 0
    shiftRegister = 0
    shiftRegWriteCount = 0


  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address >= 0x8000 then writeShiftRegister(address,value)

  override def readPRG(address: Int): Int =
    if address < 0xC000 then
      prgRomBankMode match
        case 0|1 =>
          ROM(romBankIndex & 0x1E)(address & 0x3FFF)
        case 2 =>
          ROM(0)(address & 0x3FFF)
        case 3 =>
          ROM(romBankIndex)(address & 0x3FFF)
    else
      prgRomBankMode match
        case 0|1 =>
          ROM((romBankIndex & 0x1E) + 1)(address & 0x3FFF) // clear bit 0
        case 2 =>
          ROM(romBankIndex)(address & 0x3FFF)
        case 3 =>
          ROM(ROM.length - 1)(address & 0x3FFF)

  override def readCHR(address: Int): Int =
    if chrBankMode == 0 then // 8k mode
      CHR((chr0BankIndex >> 1) % CHR.length)(address)
    else
      // 4k mode
      if address < 0x1000 then
        readCHR_4K(address,chr0BankIndex)
      else
        readCHR_4K(address,chr1BankIndex)


/*
  override def readCHR(address: Int): Int =
    if address < 0x1000 then
      if chrBankMode == 0 then // 8k mode
        CHR(((chr0BankIndex & 0xE) >> 1) % CHR.length)(address)
      else // 4k mode
        val oddBank = (chr0BankIndex & 1) > 0
        val adr = address + (if oddBank then 0x1000 else 0)
        CHR((chr0BankIndex >> 1) % CHR.length)(adr)
    else
      if chrBankMode == 0 then // 8k mode
        CHR((((chr0BankIndex & 0xE) >> 1)) % CHR.length)(address)
      else // 4k mode
        val evenBank = (chr1BankIndex & 1) == 0
        val adr = address - (if evenBank then 0x1000 else 0)
        CHR((chr1BankIndex >> 1) % CHR.length)(adr)
*/
  override def writeCHR(address: Int, value: Int): Unit =
    if ines.chrIsRAM then
      if address < 0x1000 then
        if chrBankMode == 0 then // 8k mode
          CHR((chr0BankIndex & 0xE) >> 1)(address) = value
        else // 4k mode
          val oddBank = (chr0BankIndex & 1) > 0
          val adr = address + (if oddBank then 0x1000 else 0)
          CHR(chr0BankIndex >> 1)(adr) = value
      else
        if chrBankMode == 0 then // 8k mode
          CHR((chr0BankIndex & 0xE) >> 1)(address) = value
        else // 4k mode
          val evenBank = (chr1BankIndex & 1) == 0
          val adr = address - (if evenBank then 0x1000 else 0)
          CHR(chr1BankIndex >> 1)(adr) = value
        
  private def writeShiftRegister(address: Int, value: Int) : Unit =
    val cpuClock = clk.currentCycles / 3
    val oldLastCPUClockWriteCycle = lastCPUClockWriteCycle
    lastCPUClockWriteCycle = cpuClock

    if oldLastCPUClockWriteCycle + 1 == cpuClock then
      return // ignores write

    if (value & 0x80) > 0 then
      shiftRegister = 0
      shiftRegWriteCount = 0
      ctrlReg |= 0xC
    else
      val bit = value & 1
      shiftRegister = shiftRegister >> 1 | bit << 4
      shiftRegWriteCount += 1
      if shiftRegWriteCount == 5 then
        writeReg(shiftRegister,(address >> 13) & 3)
        shiftRegister = 0
        shiftRegWriteCount = 0

  private def writeReg(sr:Int,reg:Int) : Unit =
    reg match
      case CONTROL_REG =>
        ctrlReg = sr & 0x1F
        prgRomBankMode = (ctrlReg >> 2) & 3
        chrBankMode = (ctrlReg >> 4) & 1
        sr & 3 match
          case 0 =>
            setMirroring(Mirroring.ALL_MIRROR0)
          case 1 =>
            setMirroring(Mirroring.ALL_MIRROR1)
          case 2 =>
            setMirroring(Mirroring.VERTICAL)
          case 3 =>
            setMirroring(Mirroring.HORIZONTAL)
      case PRG_BANK_REG =>
        romBankIndex = sr & 0x0F
        sramEnabled = (sr & 0x10) == 0
      case CHR_BANK_0_REG =>
        chr0BankIndex = sr & 0x1F
      case CHR_BANK_1_REG =>
        chr1BankIndex = sr & 0x1F

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeInt(shiftRegister)
    out.writeInt(shiftRegWriteCount)
    out.writeLong(lastCPUClockWriteCycle)
    out.writeInt(prgRomBankMode)
    out.writeInt(chrBankMode)
    out.writeInt(chr0BankIndex)
    out.writeInt(chr1BankIndex)
    out.writeInt(ctrlReg)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    shiftRegister = in.readInt()
    shiftRegWriteCount = in.readInt()
    lastCPUClockWriteCycle = in.readLong()
    prgRomBankMode = in.readInt()
    chrBankMode = in.readInt()
    chr0BankIndex = in.readInt()
    chr1BankIndex = in.readInt()
    ctrlReg = in.readInt()
    super.loadState(in)