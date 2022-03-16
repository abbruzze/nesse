package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_TaitoTC0690(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "TAITO TC0690"

  private[this] final val SECOND_LAST_ROM = (ROM.length - 1) << 1
  private[this] final val LAST_ROM = ((ROM.length - 1) << 1) + 1

  private[this] var prg0Bank,prg1Bank = 0
  private[this] val chrRegs = Array.ofDim[Int](6)
  private[this] var irqCounter = 0
  private[this] var irqLatch = 0
  private[this] var irqEnabled = false
  private[this] var irqReloadPending = false
  private[this] var irqPendingCounter = 0
  private[this] var a12Counter = 0
  private[this] var isTC0690 = false

  override def reset: Unit =
    java.util.Arrays.fill(chrRegs,0)
    prg0Bank = 0
    prg1Bank = 0
    irqCounter = 0
    irqLatch = 0
    irqEnabled = false
    irqReloadPending = false
    irqPendingCounter = 0
    a12Counter = 0

  override def writeCPU(address: Int, value: Int): Unit =
    import Cartridge.Mirroring.*

    super.writeCPU(address, value)
    if address < 0x8000 then return

    address & 0xE003 match
      case 0x8000 =>
        if !isTC0690 then
          if (value & 0x40) == 0 then setMirroring(VERTICAL) else setMirroring(HORIZONTAL)
          prg0Bank = value & 0x3F
        else
          prg0Bank = value
      case 0x8001 =>
        prg1Bank = value
      case 0x8002 =>
        chrRegs(0) = value
      case 0x8003 =>
        chrRegs(1) = value
      case 0xA000|0xA001|0xA002|0xA003 =>
        chrRegs(2 + (address & 3)) = value
      case 0xC000 =>
        irqLatch = value ^ 0xFF
        isTC0690 = true
      case 0xC001 =>
        irqCounter = 0
        irqReloadPending = true
        isTC0690 = true
      case 0xC002 =>
        irqEnabled = true
        isTC0690 = true
      case 0xC003 =>
        irqEnabled = false
        irqHandler(false)
        isTC0690 = true
      case 0xE000 =>
        if (value & 0x40) == 0 then setMirroring(VERTICAL) else setMirroring(HORIZONTAL)
        isTC0690 = true

  override def readPRG(address: Int): Int =
    if address < 0xA000 then
      readPRG_8K(address,prg0Bank)
    else
    if address < 0xC000 then
      readPRG_8K(address,prg1Bank)
    else
    if address < 0xE000 then
      readPRG_8K(address,SECOND_LAST_ROM)
    else
      readPRG_8K(address,LAST_ROM)

  override def readCHR(address: Int): Int =
    if address < 0x800 then
      readCHR_2K(address,chrRegs(0))
    else
    if address < 0x1000 then
      readCHR_2K(address,chrRegs(1))
    else
      readCHR_1K(address,chrRegs((address >> 10) - 2))

  private def ppuNewLine(): Unit =
    if irqCounter == 0 || irqReloadPending then
      irqCounter = irqLatch
      irqReloadPending = false
    else
      irqCounter -= 1

    if irqCounter == 0 && irqEnabled then
      irqPendingCounter = 4

  override def cpuClock(): Unit =
    if irqPendingCounter > 0 then
      irqPendingCounter -= 1
      if irqPendingCounter == 0 then
        irqHandler(true)

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
    out.writeObject(chrRegs)
    out.writeInt(prg0Bank)
    out.writeInt(prg1Bank)
    out.writeInt(irqCounter)
    out.writeInt(irqLatch)
    out.writeBoolean(irqEnabled)
    out.writeBoolean(irqReloadPending)
    out.writeInt(a12Counter)
    out.writeInt(irqPendingCounter)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(chrRegs,in)
    prg0Bank = in.readInt()
    prg1Bank = in.readInt()
    irqCounter = in.readInt()
    irqLatch = in.readInt()
    irqEnabled = in.readBoolean()
    irqReloadPending = in.readBoolean()
    a12Counter = in.readInt()
    irqPendingCounter = in.readInt()
    super.loadState(in)