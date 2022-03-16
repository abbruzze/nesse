package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_011(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  import Cartridge.*

  override val cartName : String = "Color Dreams"

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address >= 0x8000 then
      romBankIndex = value & 3
      chrBankIndex = value >> 4
