package ucesoft.nes.mappers

import ucesoft.nes.mappers.chips._24C02
import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_016(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "FCG"

  private[this] val EEPROM = ines.game.map(_.chipsType).getOrElse(Nil).contains("24C02")
  private[this] val MASK =  if EEPROM then 0x8000 else 0xE000
  private[this] val chrBank = Array.ofDim[Int](8)
  private[this] lazy val eeprom = new _24C02
  private[this] var irqEnabled = false
  private[this] var irqCounter,irqLatch = 0

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(chrBank)
    out.writeBoolean(irqEnabled)
    out.writeInt(irqCounter)
    out.writeInt(irqLatch)
    eeprom.saveState(out)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(chrBank,in)
    irqEnabled = in.readBoolean()
    irqCounter = in.readInt()
    irqLatch = in.readInt()
    eeprom.loadState(in)
    super.loadState(in)

  override def reset: Unit =
    eeprom.reset()
    java.util.Arrays.fill(chrBank,0)
    irqEnabled = false
    irqCounter = 0
    irqLatch = 0

  override def readRAM(address: Int): Int =
    if EEPROM then
      eeprom.readSDA << 4
    else
      super.readRAM(address)

  override def writeCPU(address: Int, value: Int): Unit =
    address & MASK match
      case 0x8000 | 0x6000 =>
        val adr = address & 0xF
        if adr < 8 then
          chrBank(adr) = value
        else adr match
          case 8 =>
            romBankIndex = value & 0xF
          case 9 =>
            import Cartridge.Mirroring.*
            value & 3 match
              case 0 => setMirroring(VERTICAL)
              case 1 => setMirroring(HORIZONTAL)
              case 2 => setMirroring(ALL_MIRROR0)
              case 3 => setMirroring(ALL_MIRROR1)
          case 0xA =>
            irqEnabled = (value & 1) > 0
            irqHandler(false)
            if EEPROM then irqCounter = irqLatch
          case 0xB =>
            if EEPROM then
              irqLatch = (irqLatch & 0xFF00) | value
            else
              irqCounter = (irqCounter & 0xFF00) | value
          case 0xC =>
            if EEPROM then
              irqLatch = (value << 8) | (irqLatch & 0xFF)
            else
              irqCounter = (value << 8) | (irqCounter & 0xFF)
          case 0xD if EEPROM =>
            val scl = (value & 0x20) > 0
            val sda = (value & 0x40) > 0
            eeprom.clock(scl,sda)
          case _ =>

  override def readPRG(address: Int): Int =
    if address < 0xC000 then ROM(romBankIndex)(address & 0x3FFF)
    else ROM(ROM_SIZE - 1)(address & 0x3FFF)

  override def readCHR(address: Int): Int =
    readCHR_1K(address,chrBank(address >> 10))

  override def cpuClock(): Unit =
    if irqEnabled && irqCounter > 0 then
      irqCounter -= 1
      if irqCounter == 0 then
        irqHandler(true)

  override protected def hasMemoryToSave(): Boolean = true
  override protected def saveRAM(out: ObjectOutputStream): Unit = eeprom.saveEEPROM(out)
  override protected def loadRAM(in: ObjectInputStream): Unit = eeprom.loadEEPROM(in)