package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, NESComponent, PPU}

class Mapper_185(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "185"
  private var chrEnabled = true

  override protected def writeCPU(address:Int,value:Int) : Unit =
    if address >= 0x8000 then
      val cc = value & 0x33
      chrEnabled = (cc & 0xF) != 0 && cc != 0x13
      chrBankIndex = cc & 0x3
    else
      super.writeCPU(address,value)

  override protected def readCHR(address: Int): Int =
    if chrEnabled then
      super.readCHR(address)
    else
      address & 0xFF