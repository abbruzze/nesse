package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_066(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName: String = "GxROM"

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address >= 0x8000 then
      chrBankIndex = value & 3
      romBankIndex = (value >> 4) & 3

  override protected def readPRG(address: Int) : Int =
    if address < 0xC000 then ROM(romBankIndex << 1)(address & 0x3FFF)
    else ROM((romBankIndex << 1) + 1)(address & 0x3FFF)