package ucesoft.nes.mappers

import ucesoft.nes.{ PPU, Cartridge }

class Mapper_071(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "Codemaster"

  private[this] final val isBF9097 = ines.game.map(_.chipsType).getOrElse(Nil).contains("BF9097")

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)

    if isBF9097 && address > 0x7FFF && address < 0xA000 then
      value & 0x10 match
        case 0 =>
          setMirroring(Cartridge.Mirroring.ALL_MIRROR0)
        case 0x10 =>
          setMirroring(Cartridge.Mirroring.ALL_MIRROR1)
    else
    if address >= 0xC000 then
      romBankIndex = value & 0xF

  override def readPRG(address: Int): Int =
    if address < 0xC000 then
      ROM(romBankIndex % ROM.length)(address & 0x3FFF)
    else
      ROM(ROM.length - 1)(address & 0x3FFF)

