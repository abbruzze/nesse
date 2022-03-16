package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_009(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  import Cartridge.*

  override val cartName : String = "MMC2"

  private[this] final val LAST_ROM = ((ROM.length - 1) << 1) + 1
  private[this] final val SECOND_LAST_ROM = LAST_ROM - 1
  private[this] final val THIRD_LAST_ROM = LAST_ROM - 2

  protected val chrRegs = Array.ofDim[Int](4)
  protected val latches = Array(0,2)

  override def reset: Unit =
    latches(0) = 0
    latches(1) = 2
    java.util.Arrays.fill(chrRegs,0)

  override protected def readPRG(address:Int) : Int =
    if address < 0xA000 then
      readPRG_8K(address,romBankIndex)
    else if address < 0xC000 then
      readPRG_8K(address,THIRD_LAST_ROM)
    else if address < 0xE000 then
      readPRG_8K(address,SECOND_LAST_ROM)
    else
      readPRG_8K(address,LAST_ROM)
      
  protected def cart_readPPU(address:Int,readOnly:Boolean = false) : Int = super.readPPU(address,readOnly)

  override protected def readPPU(address:Int,readOnly:Boolean = false) : Int =
    val read = super.readPPU(address,readOnly)
    address match
      case 0xFD8 => latches(0) = 0
      case 0xFE8 => latches(0) = 1
      case _ =>
        if address >= 0x1FD8 && address < 0x1FE0 then latches(1) = 2
        else
        if address >= 0x1FE8 && address < 0x1FF0 then latches(1) = 3

    read

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)

    if address < 0xA000 then return
    else if address < 0xB000 then
      romBankIndex = value & 0xF
    else if address < 0xF000 then
      val chrIndex = (address >> 12) - 0xB
      chrRegs(chrIndex) = value & 0x1F
    else
      value & 1 match
        case 0 => setMirroring(Mirroring.VERTICAL)
        case 1 => setMirroring(Mirroring.HORIZONTAL)

  override protected def readCHR(address:Int) : Int =
    val chrIndex =
    if address < 0x1000 then
      chrRegs(latches(0))
    else
      chrRegs(latches(1))

    readCHR_4K(address,chrIndex)

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(chrRegs)
    out.writeObject(latches)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(chrRegs,in)
    loadMemory(latches,in)
    super.loadState(in)
