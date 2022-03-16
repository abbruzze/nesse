package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_099(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "Vs System"
  override val listenToAllAddresses: Boolean = true

  override def writeCPU(address: Int, value: Int): Unit =
    if address == 0x4016 then
      chrBankIndex = (value >> 2) & 1
      if ROM_SIZE > 2 then
        romBankIndex = chrBankIndex << 2
    else
      super.writeCPU(address,value)

  override def readPRG(address: Int): Int =
    if address < 0xA000 then
      readPRG_8K(address,romBankIndex)
    else if address < 0xC000 then
      readPRG_8K(address,1)
    else if address  < 0xE000 then
      readPRG_8K(address,2)
    else
      readPRG_8K(address,3)
