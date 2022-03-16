package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}
import ucesoft.nes.cpu.CPU6502

class Mapper_000(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "NROM"
