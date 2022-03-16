package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_VRC2(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName: String = "VRC2"

  private[this] final val REG_22 = Array(1,0)
  private[this] final val REG_25 = Array(1,0)
  private[this] final val REG_23 = Array(0,1)

  private[this] val reg = ines.mapperID match
    case 22 => REG_22
    case 23 => REG_23
    case 25 => REG_25

  private[this] final val isVRC2a = ines.mapperID == 22
  private[this] final val isPCB350926 = ines.game.map(_.pcb.toInt).getOrElse(0) == 350926

  private[this] var prg0Bank,prg1Bank = 0
  private[this] val chrBanks = Array.ofDim[Int](8)
  private[this] var latch = 0

  override def reset: Unit =
    prg0Bank = 0
    prg1Bank = 0
    latch = 0
    java.util.Arrays.fill(chrBanks,0)

  override def readPRG(address: Int): Int =
    if address < 0xA000 then
      readPRG_8K(address,prg0Bank)
    else
    if address < 0xC000 then
      readPRG_8K(address,prg1Bank)
    else
      ROM(ROM.length - 1)(address & 0x3FFF)

  override protected def readRAM(address:Int) : Int =
    if address < 0x7000 then latch
    else openbus

  override def writeCPU(address: Int, value: Int): Unit =
    import Cartridge.Mirroring.*
    super.writeCPU(address, value)

    val A0 = if ((1 << reg(0)) & address) > 0 then 1 else 0
    val A1 = if ((1 << reg(1)) & address) > 0 then 1 else 0
    val register = A0 | A1 << 1

    address >> 12 match
      case 0x6 =>
        if isPCB350926 then latch = value
      case 0x8 =>
        prg0Bank = value & 0x1F
      case 0x9 =>
        value & 1 match
          case 0 => setMirroring(VERTICAL)
          case 1 => setMirroring(HORIZONTAL)
      case 0xA =>
        prg1Bank = value & 0x1F
      case c@(0xB|0xC|0xD|0xE) =>
        val _value = if isVRC2a then value >> 1 else value
        val chrIndex = ((c - 0xB) << 1) + (if register > 1 then 1 else 0)
        register & 1 match
          case 0 => chrBanks(chrIndex) = (chrBanks(chrIndex) & 0xF0) | (_value & 0xF)
          case 1 => chrBanks(chrIndex) = (chrBanks(chrIndex) & 0xF) | (_value & 0xF) << 4
      case _ =>

  override def writeCHR(address: Int,value:Int): Unit =
    val chrIndex = (address >> 10) & 7
    writeCHR_1K(address,chrBanks(chrIndex),value)

  override def readCHR(address: Int): Int =
    val chrIndex = (address >> 10) & 7
    readCHR_1K(address,chrBanks(chrIndex))

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeInt(prg0Bank)
    out.writeInt(prg1Bank)
    out.writeObject(chrBanks)
    out.writeInt(latch)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    prg0Bank = in.readInt()
    prg1Bank = in.readInt()
    loadMemory(chrBanks,in)
    latch = in.readInt()
    super.loadState(in)

