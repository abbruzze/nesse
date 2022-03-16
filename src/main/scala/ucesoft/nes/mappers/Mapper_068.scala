package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

class Mapper_068(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "SUNSOFT4"

  private[this] val chrRegs = Array.ofDim[Int](4)
  private[this] val lowerNameTableChrROM = Array.ofDim[Int](1024)
  private[this] val upperNameTableChrROM = Array.ofDim[Int](1024)
  private[this] var nameTableMode = 0

  override def reset: Unit =
    romBankIndex = 0
    java.util.Arrays.fill(chrRegs,0)
    nameTableMode = 0

  override def writeCPU(address: Int, value: Int): Unit =
    import Cartridge.Mirroring.*

    super.writeCPU(address, value)
    if address < 0x8000 then return

    if address < 0xC000 then
      val index = (address >> 12) - 8
      chrRegs(index) = value
    else
    if address < 0xD000 then
      val index = value | 0x80
      val chr = CHR((index >> 3)%CHR_SIZE)
      System.arraycopy(chr,(index & 7) << 10,lowerNameTableChrROM,0,1024)
    else
    if address < 0xE000 then
      val index = value | 0x80
      val chr = CHR((index >> 3)%CHR_SIZE)
      System.arraycopy(chr,(index & 7) << 10,upperNameTableChrROM,0,1024)
    else
    if address < 0xF000 then
      if (value & 0x10) == 0 then
        value & 3 match
          case 0 => setMirroring(VERTICAL)
          case 1 => setMirroring(HORIZONTAL)
          case 2 => setMirroring(ALL_MIRROR0)
          case 3 => setMirroring(ALL_MIRROR1)
      else
        value & 3 match
          case 0 =>
            ppu.nt_2000 = lowerNameTableChrROM
            ppu.nt_2400 = upperNameTableChrROM
            ppu.nt_2800 = ppu.nt_2000
            ppu.nt_2C00 = ppu.nt_2400
          case 1 =>
            ppu.nt_2000 = lowerNameTableChrROM
            ppu.nt_2400 = ppu.nt_2000
            ppu.nt_2800 = upperNameTableChrROM
            ppu.nt_2C00 = ppu.nt_2800
          case 2 =>
            ppu.nt_2000 = lowerNameTableChrROM
            ppu.nt_2400 = lowerNameTableChrROM
            ppu.nt_2800 = lowerNameTableChrROM
            ppu.nt_2C00 = lowerNameTableChrROM
          case 3 =>
            ppu.nt_2000 = upperNameTableChrROM
            ppu.nt_2400 = upperNameTableChrROM
            ppu.nt_2800 = upperNameTableChrROM
            ppu.nt_2C00 = upperNameTableChrROM
    else
      romBankIndex = value & 0xF
      sramEnabled = (value & 0x10) > 0

  override def readPRG(address: Int): Int =
    if address < 0xC000 then
      ROM(romBankIndex % ROM_SIZE)(address & 0x3FFF)
    else
      ROM(ROM_SIZE - 1)(address & 0x3FFF)

  override def readCHR(address: Int): Int =
    readCHR_2K(address,chrRegs(address >> 11))



