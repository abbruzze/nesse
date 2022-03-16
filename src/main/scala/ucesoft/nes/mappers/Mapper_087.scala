package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, NESComponent, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_087(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "87"

  override protected def writeCPU(address:Int,value:Int) : Unit =
    if address >= 0x6000 && address < 0x8000 then
      chrBankIndex = (value & 1) << 1 | (value & 2) >> 1
    else
      super.writeCPU(address,value)