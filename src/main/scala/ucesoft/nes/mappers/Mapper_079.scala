package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_079(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "NINA-003-006"

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)

    if (address & 0xE100) == 0x4100 then
      chrBankIndex = value & 7
      romBankIndex = (value >> 3) & 1
