package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_004(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  import Cartridge.*

  override val cartName : String = "MMC3"

  private[this] final val SECOND_LAST_ROM = (ROM.length - 1) << 1
  private[this] final val LAST_ROM = ((ROM.length - 1) << 1) + 1

  private[this] val R = Array(0,2,4,5,6,7,0,0)
  private[this] var selectedR = 0
  private[this] var prgRomBankMode = 0
  private[this] var chrA12Inversion = 0
  private[this] var irqCounter = 0
  private[this] var irqLatch = 0
  private[this] var irqEnabled = false
  private[this] var irqReloadPending = false
  private[this] var a12Counter = 0
  private[this] var irqPendingCounter = 0

  override def reset: Unit =
    System.arraycopy(Array(0,2,4,5,6,7,0,0),0,R,0,8)
    selectedR = 0
    prgRomBankMode = 0
    chrA12Inversion = 0
    irqCounter = 0
    irqLatch = 0
    irqEnabled = false
    irqReloadPending = false
    a12Counter = 0
    irqPendingCounter = 0
  
  override protected def initStaticRam() : Array[Int] = Array.ofDim[Int](0x2000)

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address < 0x8000 then return

    val even = (address & 1) == 0
    if address < 0xA000 then
      if even then
        selectedR = value & 7
        prgRomBankMode = (value >> 6) & 1
        chrA12Inversion = (value >> 7) & 1
      else
        R(selectedR) = value
        if selectedR == 0 || selectedR == 1 then R(selectedR) >>= 1 //R(selectedR) &= 0xFE // ignore bit 0
        else
        if selectedR == 6 || selectedR == 7 then R(selectedR) &= 0x3F // ignore top two bits
    else
    if address < 0xC000 then
      if even then
        value & 1 match
          case 0 =>
            setMirroring(Mirroring.VERTICAL)
          case 1 =>
            setMirroring(Mirroring.HORIZONTAL)
      else
        sramReadOnly = (value & 0x40) > 0
        sramEnabled = (value & 0x80) > 0
    else
    if address < 0xE000 then
      if even then
        irqLatch = value
      else
        irqReloadPending = true
    else
      if even then
        irqEnabled = false
        irqHandler(false)
      else
        irqEnabled = true

  override def readPRG(address: Int): Int =
    if address < 0xA000 then
      prgRomBankMode match
        case 0 => readPRG_8K(address,R(6))
        case 1 => readPRG_8K(address,SECOND_LAST_ROM)
    else
    if address < 0xC000 then
      readPRG_8K(address,R(7))
    else
    if address < 0xE000 then
      prgRomBankMode match
        case 1 => readPRG_8K(address,R(6))
        case 0 => readPRG_8K(address,SECOND_LAST_ROM)
    else
      readPRG_8K(address,LAST_ROM)

  override def readCHR(address: Int): Int =
    chrA12Inversion match
      case 0 =>
        if address < 0x800 then readCHR_2K(address,R(0))
        else
        if address < 0x1000 then readCHR_2K(address,R(1))
        else
        if address < 0x1400 then readCHR_1K(address,R(2))
        else
        if address < 0x1800 then readCHR_1K(address,R(3))
        else
        if address < 0x1C00 then readCHR_1K(address,R(4))
        else readCHR_1K(address,R(5))
      case 1 =>
        if address < 0x400 then readCHR_1K(address,R(2))
        else
        if address < 0x800 then readCHR_1K(address,R(3))
        else
        if address < 0xC00 then readCHR_1K(address,R(4))
        else
        if address < 0x1000 then readCHR_1K(address,R(5))
        else
        if address < 0x1800 then readCHR_2K(address,R(0))
        else
          readCHR_2K(address,R(1))

  override def writeCHR(address: Int,value:Int): Unit =
    if ines.chrIsRAM then
      chrA12Inversion match
        case 0 =>
          if address < 0x800 then writeCHR_2K(address,R(0),value)
          else
          if address < 0x1000 then writeCHR_2K(address,R(1),value)
          else
          if address < 0x1400 then writeCHR_1K(address,R(2),value)
          else
          if address < 0x1800 then writeCHR_1K(address,R(3),value)
          else
          if address < 0x1C00 then writeCHR_1K(address,R(4),value)
          else writeCHR_1K(address,R(5),value)
        case 1 =>
          if address < 0x400 then writeCHR_1K(address,R(2),value)
          else
          if address < 0x800 then writeCHR_1K(address,R(3),value)
          else
          if address < 0xC00 then writeCHR_1K(address,R(4),value)
          else
          if address < 0x1000 then writeCHR_1K(address,R(5),value)
          else
          if address < 0x1800 then writeCHR_2K(address,R(0),value)
          else
            writeCHR_2K(address,R(1),value)
  
  override def cpuClock(): Unit =
    if irqPendingCounter > 0 then
      irqPendingCounter -= 1
      if irqPendingCounter == 0 then
        irqHandler(true)

  private def ppuNewLine(): Unit =
    if irqCounter == 0 || irqReloadPending then
      irqCounter = irqLatch
      irqReloadPending = false
    else
      irqCounter -= 1

    if irqCounter == 0 && irqEnabled then
      irqPendingCounter = 2


  override def readPPU(address: Int,readOnly:Boolean): Int =
    if !readOnly then ppuAddressOnBus(address)
    super.readPPU(address)

  override def writePPU(address: Int, value: Int): Unit =
    super.writePPU(address, value)
    ppuAddressOnBus(address)
  
  override def ppuAddressOnBus(addressOnBus:Int): Unit =
    if a12Counter > 0 then a12Counter -= 1
    val a12 = (addressOnBus & 0x1000) != 0

    if a12 then
      if a12Counter == 0 then
        ppuNewLine()
      a12Counter = 8

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(R)
    out.writeInt(selectedR)
    out.writeInt(prgRomBankMode)
    out.writeInt(chrA12Inversion)
    out.writeInt(irqCounter)
    out.writeInt(irqLatch)
    out.writeBoolean(irqEnabled)
    out.writeBoolean(irqReloadPending)
    out.writeInt(a12Counter)
    out.writeInt(irqPendingCounter)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(R,in)
    selectedR = in.readInt()
    prgRomBankMode = in.readInt()
    chrA12Inversion = in.readInt()
    irqCounter = in.readInt()
    irqLatch = in.readInt()
    irqEnabled = in.readBoolean()
    irqReloadPending = in.readBoolean()
    a12Counter = in.readInt()
    irqPendingCounter = in.readInt()
    super.loadState(in)
