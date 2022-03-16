package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_VRC4(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName: String = "VRC4"

  private[this] final val REG_21 = Array(Array(1,2),Array(6,7))
  private[this] final val REG_25 = Array(Array(1,0),Array(3,2))
  private[this] final val REG_23 = Array(Array(2,3),Array(0,1))
  private[this] final val VRC4_PCB_MAP = Map(
    352398 -> REG_21,
    351406 -> REG_25,
    352889 -> REG_21,
    352400 -> REG_25,
    352396 -> REG_23)
  private[this] final val VRC2_PCB_MAP = Map(
    351618 -> 22,
    351948 -> 25
  )

  private[this] final val SECOND_LAST_ROM = (ROM.length - 1) << 1
  private[this] final val LAST_ROM = ((ROM.length - 1) << 1) + 1
  private[this] var reg = REG_21
  private[this] var swapMode = false
  private[this] var prg0Bank, prg1Bank = 0
  private[this] val chrBanks = Array.ofDim[Int](8)
  private[this] var irqLatch,irqControl,irqCounter,irqPrescaler = 0

  // CONSTRUCTOR ======================
  findReg()
  // ==================================

  override def reset: Unit =
    reg = REG_21
    swapMode = false
    prg0Bank = 0
    prg1Bank = 0
    java.util.Arrays.fill(chrBanks,0)
    irqLatch = 0
    irqControl = 0
    irqCounter = 0
    irqPrescaler = 0

  private def findReg(): Unit =
    // first, check pcb
    if ines.game.isDefined then
      VRC4_PCB_MAP get ines.game.get.pcb.toInt match
        case Some(r) =>
          reg = r
          return
        case None =>
          if ines.game.get.chipsType.contains("VRC2") then
            throw new MapperFactory.ChangeMapperIDException(22)
    // check if it's a VRC2
    VRC2_PCB_MAP get ines.mapperID match
      case Some(id) =>
        throw new MapperFactory.ChangeMapperIDException(id)
      case None =>
    // pcb not found try with mapperID
    reg = ines.mapperID match
      case 21 => REG_21
      case 23 => REG_23
      case 25 => REG_25
      case id => throw new IllegalArgumentException(s"Cannot initialize VRC4 with mapperID $id")

  override def cpuClock(): Unit =
    if (irqControl & 2) > 0 then // IRQ enabled
      if (irqControl & 4) == 0 then // scanline mode
        irqPrescaler -= 3
        if irqPrescaler <= 0 then
          irqPrescaler += 341
          clockIRQ()
      else
        clockIRQ()


  inline private def clockIRQ(): Unit =
    if irqCounter == 0xFF then
      irqCounter = irqLatch
      irqHandler(true)
    else
      irqCounter = (irqCounter + 1) & 0xFF

  override def readPRG(address: Int): Int =
    if address < 0xA000 then
      if swapMode then
        readPRG_8K(address,SECOND_LAST_ROM)
      else
        readPRG_8K(address,prg0Bank)
    else
    if address < 0xC000 then
      readPRG_8K(address,prg1Bank)
    else
    if address < 0xE000 then
      if swapMode then
        readPRG_8K(address,prg0Bank)
      else
        readPRG_8K(address,SECOND_LAST_ROM)
    else
      readPRG_8K(address,LAST_ROM)


  override def writeCHR(address: Int,value:Int): Unit =
    val chrIndex = (address >> 10) & 7
    writeCHR_1K(address,chrBanks(chrIndex),value)

  override def readCHR(address: Int): Int =
    val chrIndex = (address >> 10) & 7
    readCHR_1K(address,chrBanks(chrIndex))

  override def writeCPU(address: Int, value: Int): Unit =
    import Cartridge.Mirroring.*
    super.writeCPU(address, value)

    val A0 = if ((1 << reg(0)(0)) & address) > 0 || ((1 << reg(1)(0)) & address) > 0 then 1 else 0
    val A1 = if ((1 << reg(0)(1)) & address) > 0 || ((1 << reg(1)(1)) & address) > 0 then 1 else 0
    val register = A0 | A1 << 1

    address >> 12 match
      case 0x8 =>
        prg0Bank = value & 0x1F
      case 0x9 =>
        register match
          case 0 => // mirroring
            value & 3 match
              case 0 => setMirroring(VERTICAL)
              case 1 => setMirroring(HORIZONTAL)
              case 2 => setMirroring(ALL_MIRROR0)
              case 3 => setMirroring(ALL_MIRROR1)
          case 2 => // swap mode
            swapMode = (value & 2) > 0
          case _ =>
      case 0xA =>
        prg1Bank = value & 0x1F
      case c@(0xB|0xC|0xD|0xE) =>
        val chrIndex = ((c - 0xB) << 1) + (if register > 1 then 1 else 0)
        register & 1 match
          case 0 => chrBanks(chrIndex) = (chrBanks(chrIndex) & 0x1F0) | (value & 0xF)
          case 1 => chrBanks(chrIndex) = (chrBanks(chrIndex) & 0xF) | (value & 0x1F) << 4
      case 0xF =>
        register match
          case 0 => irqLatch = (irqLatch & 0xF0) | value & 0xF
          case 1 => irqLatch = (irqLatch & 0xF) | (value & 0xF) << 4
          case 2 =>
            irqControl = value & 3
            // ack pending IRQ
            irqHandler(false)
            if (irqControl & 2) > 0 then // enabled
              irqCounter = irqLatch
              irqPrescaler = 341
          case 3 =>
            // ack pending IRQ
            irqHandler(false)
            irqControl = (irqControl & 5) | (irqControl & 1) << 1
      case _ =>

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(reg(0))
    out.writeObject(reg(1))
    out.writeBoolean(swapMode)
    out.writeInt(prg0Bank)
    out.writeInt(prg1Bank)
    out.writeObject(chrBanks)
    out.writeInt(irqLatch)
    out.writeInt(irqControl)
    out.writeInt(irqCounter)
    out.writeInt(irqPrescaler)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(reg(0),in)
    loadMemory(reg(1),in)
    swapMode = in.readBoolean()
    prg0Bank = in.readInt()
    prg1Bank = in.readInt()
    loadMemory(chrBanks,in)
    irqLatch = in.readInt()
    irqControl = in.readInt()
    irqCounter = in.readInt()
    irqPrescaler = in.readInt()
    super.loadState(in)