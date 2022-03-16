package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_010(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Mapper_009(ppu,ines,irqHandler):
  import Cartridge.*

  override val cartName : String = "MMC4"

  private[this] final val LAST_ROM = ROM.length - 1

  override protected def readPRG(address:Int) : Int =
    if address < 0xC000 then
      ROM(romBankIndex % ROM.length)(address & 0x3FFF)
    else
      ROM(LAST_ROM)(address & 0x3FFF)

  override protected def readPPU(address:Int,readOnly:Boolean = false) : Int =
    val read = cart_readPPU(address,readOnly)

    address & 0xFFF8 match
      case 0x0FD8 => latches(0) = 0
      case 0x0FE8 => latches(0) = 1
      case 0x1FD8 => latches(1) = 2
      case 0x1FE8 => latches(1) = 3
      case _ =>

    read
