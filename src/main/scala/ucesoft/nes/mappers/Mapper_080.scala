package ucesoft.nes.mappers

import ucesoft.nes.Cartridge.Mirroring
import ucesoft.nes.{Cartridge, PPU}

class Mapper_080(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "X1-005"
  private val chrRegs = Array.ofDim[Int](6)
  private val prgRegs = Array.ofDim[Int](3)
  private final val LAST_ROM = ((ROM.length - 1) << 1) + 1

  override def initStaticRam(): Array[Int] = Array.ofDim[Int](128)
  
  override protected def readPRG(address: Int) : Int =
    if address < 0xA000 then readPRG_8K(address,prgRegs(0))
    else if address < 0xC000 then readPRG_8K(address,prgRegs(1))
    else if address < 0xE000 then readPRG_8K(address,prgRegs(2))
    else readPRG_8K(address,LAST_ROM)

  override protected def readCHR(address: Int): Int =
    if address < 0x800 then
      readCHR_1K(address,chrRegs(0))
    else if address < 0x1000 then
      readCHR_1K(address,chrRegs(1))
    else if address < 0x1400 then
      readCHR_1K(address, chrRegs(2))
    else if address < 0x1800 then
      readCHR_1K(address, chrRegs(3))
    else if address < 0x1C00 then
      readCHR_1K(address, chrRegs(4))
    else readCHR_1K(address, chrRegs(5))

  override def readCPU(address: Int): Int =
    if address < 0x7F00 then openbus
    else if address < 0x8000 then
      if sramEnabled then
        SRAM(address & 0x7F)
      else openbus
    else super.readCPU(address)
  override protected def writeCPU(address: Int, value: Int): Unit =
    if address >= 0x7EF0 && address < 0x7EF6 then
      chrRegs(address & 0xF) = value
    else if address >= 0x7EF6 && address < 0x7EF8 then
      value & 1 match
        case 0 => setMirroring(Mirroring.HORIZONTAL)
        case 1 => setMirroring(Mirroring.VERTICAL)
    else if address >= 0x7EF8 && address < 0x7EFA then
      sramEnabled = value == 0xA3
    else if address >= 0x7EFA && address < 0x7F00 then
      prgRegs(((address & 0xF) - 0xA) >> 1) = value
    else if address >= 0x7F00 && address < 0x8000 then
      if sramEnabled then
        SRAM(address & 0x7F) = value