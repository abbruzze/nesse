package ucesoft.nes

abstract class Chip extends NESComponent {
  val id: ChipID
  val componentType = NESComponentType.CHIP
  //val componentID = componentType.toString
}

enum ChipID:
  case CPU, PPU, PPU_RO, APU
