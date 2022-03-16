package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_107(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "Magic Corp A"

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)

    if address >= 0x8000 then
      romBankIndex = value >> 1
      chrBankIndex = value

  override def readPRG(address: Int): Int =  
    var romIndex = romBankIndex << 1
    if address >= 0xC000 then romIndex += 1
    ROM(romIndex)(address & 0x3FFF)
