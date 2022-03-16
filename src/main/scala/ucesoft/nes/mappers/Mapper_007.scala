package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_007(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "AxROM"
  
  override protected def readPRG(address: Int) : Int =
    if address < 0xC000 then ROM(romBankIndex << 1)(address & 0x3FFF) // banked
    else ROM((romBankIndex << 1) + 1)(address & 0x3FFF)

  override protected def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address >= 0x8000 then
      romBankIndex = value & 7
      if (value & 0x10) > 0 then setMirroring(Cartridge.Mirroring.ALL_MIRROR1) else setMirroring(Cartridge.Mirroring.ALL_MIRROR0)
