package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_030(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  import Cartridge.Mirroring.*
  override val cartName : String = "UNROM512"
  private val checkMirroring = mirroring != HORIZONTAL && mirroring != VERTICAL
  
  override protected def readPRG(address: Int) : Int =
    if address < 0xC000 then ROM(romBankIndex)(address & 0x3FFF) // banked
    else ROM(ROM.length - 1)(address & 0x3FFF)

  override protected def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address >= 0x8000 then
      romBankIndex = value & 0x1F
      chrBankIndex = (value >> 5) & 3
      if checkMirroring then
        if (value & 0x80) == 0 then
          setMirroring(ALL_MIRROR0)
        else
          setMirroring(ALL_MIRROR1)
