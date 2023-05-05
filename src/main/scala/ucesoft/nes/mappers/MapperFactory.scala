package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}
import fds.{Mapper_FDS, FDS }

object MapperFactory:
  val mappers = Set(0,1,2,3,4,5,7,9,10,11,13,16,18,19,20,21,22,23,25,30,33,48,65,66,68,69,71,73,79,80,87,99,107,152,185,206,210)
  class ChangeMapperIDException(val mapperID:Int) extends Exception

  def mapperFrom(ines:Cartridge.iNES,irqHandler: Boolean => Unit,ppu : PPU) : Cartridge =
    var id = ines.mapperID
    var found = false
    var cart : Cartridge = null

    while !found do
      try
        cart = id match
          case 0 => Mapper_000(ppu,ines,irqHandler)
          case 1 => Mapper_001(ppu,ines,irqHandler)
          case 2 => Mapper_002(ppu,ines,irqHandler)
          case 3 => Mapper_003(ppu,ines,irqHandler)
          case 4 => Mapper_004(ppu,ines,irqHandler)
          case 5 => Mapper_005(ppu,ines,irqHandler)
          case 7 => Mapper_007(ppu,ines,irqHandler)
          case 9 => Mapper_009(ppu,ines,irqHandler)
          case 10 => Mapper_010(ppu,ines,irqHandler)
          case 11 => Mapper_011(ppu,ines,irqHandler)
          case 13 => Mapper_013(ppu,ines,irqHandler)
          case 16 => Mapper_016(ppu,ines,irqHandler)
          case 18 => Mapper_018(ppu,ines,irqHandler)
          case 19 => Mapper_019(ppu,ines,irqHandler)
          case 20 =>
            if FDS.getBiosROM() == null then
              throw new IllegalArgumentException("FDS BIOS rom not set")
            Mapper_FDS(FDS.getBiosROM(),ppu,irqHandler)
          case 21|23|25 => Mapper_VRC4(ppu,ines,irqHandler)
          case 22 => Mapper_VRC2(ppu,ines,irqHandler)
          case 30 => Mapper_030(ppu,ines,irqHandler)
          case 33|48 => Mapper_TaitoTC0690(ppu,ines,irqHandler)
          case 65 => Mapper_065(ppu,ines,irqHandler)
          case 66 => Mapper_066(ppu,ines,irqHandler)
          case 68 => Mapper_068(ppu,ines,irqHandler)
          case 69 => Mapper_069(ppu,ines,irqHandler)
          case 71 => Mapper_071(ppu,ines,irqHandler)
          case 73 => Mapper_073(ppu,ines,irqHandler)
          case 79 => Mapper_079(ppu,ines,irqHandler)
          case 80 => Mapper_080(ppu,ines,irqHandler)
          case 87 => Mapper_087(ppu,ines,irqHandler)
          case 99 => Mapper_099(ppu,ines,irqHandler)
          case 107 => Mapper_107(ppu,ines,irqHandler)
          case 152 => Mapper_152(ppu,ines,irqHandler)
          case 185 => Mapper_185(ppu,ines,irqHandler)
          case 206 => Mapper_206(ppu,ines,irqHandler)
          case 210 => Mapper_210(ppu,ines,irqHandler)
          case m => throw new IllegalArgumentException(s"Mapper $m not supported")
        found = true
      catch
        case cme: ChangeMapperIDException =>
          id = cme.mapperID
    cart


