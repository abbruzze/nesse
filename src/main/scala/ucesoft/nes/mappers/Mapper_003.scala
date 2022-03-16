package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_003(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "CNROM"
  
  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address >= 0x8000 then
      chrBankIndex = value & 0x3