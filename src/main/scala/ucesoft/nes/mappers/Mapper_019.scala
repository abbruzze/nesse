package ucesoft.nes.mappers

import ucesoft.nes.mappers.sound.Namco163
import ucesoft.nes.{Cartridge, NESComponent, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_019(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "NAMCO 129/163"
  override val hasAudio = true

  private[this] final val chrRomWriteable = ines.chrIsRAM

  private[this] final val LAST_PRG_BANK = (ROM.length << 1) - 1
  private[this] var irqCounter = 0
  private[this] var irqEnabled = false
  private[this] val chrRegs = Array.ofDim[Int](12)
  private[this] val chrRAMEnabled = Array(false,false)
  private[this] val writeProtect = Array(false,false,false,false)
  private[this] val prgRomBank = Array(0,0,0)
  private[this] val audio = new Namco163

  // CONSTRUCTOR
  customNTHandling = true

  override def reset: Unit =
    audio.reset()
    irqCounter = 0
    irqEnabled = false
    java.util.Arrays.fill(chrRegs,0)
    java.util.Arrays.fill(chrRAMEnabled,false)
    java.util.Arrays.fill(writeProtect,false)
    java.util.Arrays.fill(prgRomBank,0)

  override def getAudioSample(): Double = audio.getSample
  override def getAudioRatio(): Double = 0.60

  override def saveState(out: ObjectOutputStream): Unit =
    audio.saveState(out)
    out.writeInt(irqCounter)
    out.writeBoolean(irqEnabled)
    out.writeObject(chrRegs)
    out.writeObject(chrRAMEnabled)
    out.writeObject(writeProtect)
    out.writeObject(prgRomBank)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    audio.loadState(in)
    irqCounter = in.readInt()
    irqEnabled = in.readBoolean()
    NESComponent.loadMemory(chrRegs,in)
    NESComponent.loadMemory(chrRAMEnabled,in)
    NESComponent.loadMemory(writeProtect,in)
    NESComponent.loadMemory(prgRomBank,in)
    super.loadState(in)

  override def cpuClock(): Unit =
    audio.clock()
    if irqEnabled && irqCounter < 0x7FFF then
      irqCounter += 1
      if irqCounter == 0x7FFF then
        irqHandler(true)

  override def readCPU(address: Int): Int =
    openbus = address & 0xF800 match
      case 0x4800 =>
        audio.readDataPort()
      case 0x5000 =>
        irqCounter & 0xFF
      case 0x5800 =>
        (irqCounter >> 8) & 0x7F | (if irqEnabled then 0x80 else 0)
      case _ =>
        super.readCPU(address)
    openbus

  override def writeCPU(address: Int, value: Int): Unit =
    openbus = value
    if address >= 0x6000 && address < 0x8000 && SRAM != null && !writeProtect((address >> 11) - 0xC) then
      SRAM(address & 0x1FFF) = value
    else if address >= 0x4800 then
      val adr = address & 0xF800
      if adr == 0x4800 then
        audio.writeDataPort(value)
      else
        if adr == 0x5000 then
          irqCounter = (irqCounter & 0xFF00) | value
          irqHandler(false)
        else if adr == 0x5800 then
          irqCounter = (value & 0x7F) << 8 | (irqCounter & 0xFF)
          irqEnabled = (value & 0x80) > 0
          irqHandler(false)
        else if adr >= 0x8000 && adr <= 0xD800 then
          chrRegs((adr >> 11) - 0x10) = value
        else if adr == 0xE000 then
          prgRomBank(0) = value & 0x3F
          audio.enableSound((value & 0x40) == 0)
        else if adr == 0xE800 then
          prgRomBank(1) = value & 0x3F
          chrRAMEnabled(0) = (value & 0x40) == 0
          chrRAMEnabled(1) = (value & 0x80) == 0
        else if adr == 0xF000 then
          prgRomBank(2) = value & 0x3F
        else if adr == 0xF800 then
          audio.setAddressPort(value)
          val wpEnabled = (value & 0xF0) == 0x40
          var b = 0
          while b < 3 do
            writeProtect(b) = if wpEnabled then (value & (1 << b)) > 0 else false
            b += 1

  override protected def readPRG(address: Int) : Int =
    if address < 0xE000 then
      readPRG_8K(address,prgRomBank((address >> 13) - 0x4))
    else
      readPRG_8K(address,LAST_PRG_BANK)

  override def readCHR(address: Int): Int =
    val bank = chrRegs(address >> 10)
    val ntRAM = chrRAMEnabled(address >> 12)
    if bank < 0xE0 || !ntRAM then
      readCHR_1K(address,bank)
    else
      if (bank & 1) == 0 then
        ppu.pt0(address & 0x3FF)
      else
        ppu.pt1(address & 0x3FF)

  override def readNT(address: Int,index:Int): Int =
    val bank = chrRegs(index + 8)
    if bank < 0xE0 then
      readCHR_1K(address,bank)
    else
      if (bank & 1) == 0 then
        ppu.pt0(address & 0x3FF)
      else
        ppu.pt1(address & 0x3FF)

  override def writeNT(address: Int, index: Int, value: Int): Unit =
    val bank = chrRegs(index + 8)
    if bank < 0xE0 && chrRomWriteable then
      writeCHR_1K(address,bank,value)
    else
      if (bank & 1) == 0 then
        ppu.pt0(address & 0x3FF) = value
      else
        ppu.pt1(address & 0x3FF) = value