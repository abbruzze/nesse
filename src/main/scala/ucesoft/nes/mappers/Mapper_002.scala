package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_002(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "UxROM"
  
  override protected def readPRG(address: Int) : Int =
    if address < 0xC000 then ROM(romBankIndex)(address & 0x3FFF) // banked
    else ROM(ROM.length - 1)(address & 0x3FFF)

  override protected def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address >= 0x8000 then romBankIndex = value & 7
