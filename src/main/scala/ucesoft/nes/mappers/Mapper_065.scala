package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_065(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "IREM_H3001"

  private[this] final val SECOND_LAST_ROM = (ROM.length - 1) << 1
  private[this] final val LAST_ROM = ((ROM.length - 1) << 1) + 1
  private[this] var prgReg0 = 0
  private[this] var prgReg1 = 1
  private[this] var prgMode = 0
  private[this] val chrRegs = Array.ofDim[Int](8)
  private[this] var irqEnabled = false
  private[this] var irqLatchCounter,irqCounter = 0

  override def reset: Unit =
    prgReg0 = 0
    prgReg1 = 1
    prgMode = 0
    java.util.Arrays.fill(chrRegs,0)
    irqEnabled = false
    irqCounter = 0
    irqLatchCounter = 0

  override def cpuClock(): Unit =
    if irqEnabled && irqCounter > 0 then
      irqCounter -= 1
      if irqCounter == 0 then
        irqHandler(true)

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)

    address match
      case 0x8000 => prgReg0 = value
      case 0xA000 => prgReg1 = value
      case 0xB000|0xB001|0xB002|0xB003|0xB004|0xB005|0xB006|0xB007 =>
        chrRegs(address & 7) = value
      case 0x9000 =>
        prgMode = value >> 7
      case 0x9001 =>
        setMirroring(if (value & 0x80) == 0 then Cartridge.Mirroring.VERTICAL else Cartridge.Mirroring.HORIZONTAL)
      case 0x9003 =>
        irqEnabled = (value & 0x80) > 0
        irqHandler(false)
      case 0x9004 =>
        irqCounter = irqLatchCounter
        irqHandler(false)
      case 0x9005 =>
        irqLatchCounter = (irqLatchCounter & 0xFF) | value << 8
      case 0x9006 =>
        irqLatchCounter = (irqLatchCounter & 0xFF00) | value

  override def readPRG(address: Int): Int =
    if address < 0xA000 then
      if prgMode == 0 then
        readPRG_8K(address,prgReg0)
      else
        readPRG_8K(address,SECOND_LAST_ROM)
    else
    if address < 0xC000 then
      readPRG_8K(address,prgReg1)
    else
    if address < 0xE000 then
      if prgMode == 1 then
        readPRG_8K(address,prgReg0)
      else
        readPRG_8K(address,SECOND_LAST_ROM)
    else
      readPRG_8K(address,LAST_ROM)

  override def readCHR(address: Int): Int =
    val index = (address >> 10) & 7
    readCHR_1K(address,chrRegs(index))

  override def writeCHR(address: Int, value: Int): Unit =
    val index = (address >> 10) & 7
    writeCHR_1K(address,chrRegs(index),value)

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeInt(prgReg0)
    out.writeInt(prgReg1)
    out.write(prgMode)
    out.writeObject(chrRegs)
    out.writeBoolean(irqEnabled)
    out.writeInt(irqLatchCounter)
    out.writeInt(irqCounter)

  override def loadState(in: ObjectInputStream): Unit =
    prgReg0 = in.readInt()
    prgReg1 = in.readInt()
    prgMode = in.readInt()
    loadMemory(chrRegs,in)
    irqEnabled = in.readBoolean()
    irqLatchCounter = in.readInt()
    irqCounter = in.readInt()


