package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_013(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "CPROM"

  override protected def checkCHRRom(rom:Array[Array[Int]]): Array[Array[Int]] =
    if rom.length != 2 then
      Array.ofDim[Int](2,8192)
    else
      rom

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)
    if address > 0x7FFF then
      chrBankIndex = value & 3

  override def readCHR(address: Int): Int =
    if address < 0x1000 then
      readCHR_4K(address,0)
    else
      readCHR_4K(address,chrBankIndex)

  override def writeCHR(address: Int, value: Int): Unit =
    if address < 0x1000 then
      writeCHR_4K(address,0,value)
    else
      writeCHR_4K(address,chrBankIndex,value)
