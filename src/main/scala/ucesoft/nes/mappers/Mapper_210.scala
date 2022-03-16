package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, NESComponent, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_210(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "Namco_175_340"
  private val _175 = ines.game.map(_.mapperType.contains("175")).getOrElse(false)
  private val prgRegs = Array(0,0,0)
  private val chrRegs = Array.ofDim[Int](8)
  private final val LAST_8BANK = (ROM.length << 1) - 1

  override protected def readRAM(address:Int) : Int =
    if _175 && sramEnabled then
      SRAM(address & 0x7FF)
    else
      openbus

  override protected def writeCPU(address:Int,value:Int) : Unit =
    openbus = value
    if address < 0x6000 then {}
    else if address >= 0x6000 && address < 0x8000 then
      if _175 && sramEnabled then
        SRAM(address & 0x7FF) = value
    else if address < 0xC000 then
      val index = (((address >> 12) - 8) << 1) + ((address >> 11) & 1)
      chrRegs(index) = value
    else if address < 0xC800 then
      sramEnabled = (value & 1) > 0
    else if address >= 0xE000 && address < 0xE800 then
      prgRegs(0) = value & 0x3F
      import Cartridge.Mirroring.*
      if !_175 then
        value >> 6 match
          case 0 =>
            setMirroring(ALL_MIRROR0)
          case 1 =>
            setMirroring(VERTICAL)
          case 2 =>
            setMirroring(ALL_MIRROR1)
          case 3 =>
            setMirroring(HORIZONTAL)
    else if address >= 0xE800 && address < 0xF000 then
      prgRegs(1) = value & 0x3F
    else if address >= 0xF000 && address < 0xF800 then
      prgRegs(2) = value & 0x3F

  
  override protected def readPRG(address: Int) : Int =
    if address < 0xA000 then
      readPRG_8K(address,prgRegs(0))
    else if address < 0xC000 then
      readPRG_8K(address,prgRegs(1))
    else if address < 0xE000 then
      readPRG_8K(address,prgRegs(2))
    else
      readPRG_8K(address,LAST_8BANK)

  override protected def readCHR(address: Int): Int =
    readCHR_1K(address,chrRegs(address >> 10))

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(prgRegs)
    out.writeObject(chrRegs)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    NESComponent.loadMemory(prgRegs,in)
    NESComponent.loadMemory(chrRegs,in)
    super.loadState(in)
