package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_152(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "Bandai 152"

  override def writeCPU(address: Int, value: Int): Unit =
    if address < 0x8000 then
      super.writeCPU(address,value)
    else
      import Cartridge.Mirroring.*
      value >> 7 match
        case 0 =>
          setMirroring(ALL_MIRROR0)
        case 1 =>
          setMirroring(ALL_MIRROR1)

      romBankIndex = (value >> 4) & 7
      chrBankIndex = value & 0xF

  override def readPRG(address: Int): Int =
    if address < 0xC000 then
      ROM(romBankIndex)(address & 0x3FFF)
    else
      ROM(ROM_SIZE - 1)(address & 0x3FFF)