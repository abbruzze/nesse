package ucesoft.nes.mappers

import ucesoft.nes.{Cartridge, PPU}

import java.io.{ObjectInputStream, ObjectOutputStream}

class Mapper_206(ppu : PPU,ines:Cartridge.iNES,irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName: String = "MIMIC"

  private[this] val reg = Array.ofDim[Int](8)
  private[this] var selectedReg = 0

  private[this] val VS_SECURITY_RBIBASEBALL = Array(
    0x00, 0x00, 0x00, 0x00, 0xB4, 0x00, 0x00, 0x00,
    0x00, 0x6F, 0x00, 0x00, 0x00, 0x00, 0x94, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  )
  private[this] val VS_SECURITY_TKOBOXING = Array(
    0xFF, 0xBF, 0xB7, 0x97, 0x97, 0x17, 0x57, 0x4F,
    0x6F, 0x6B, 0xEB, 0xA9, 0xB1, 0x90, 0x94, 0x14,
    0x56, 0x4E, 0x6F, 0x6B, 0xEB, 0xA9, 0xB1, 0x90,
    0xD4, 0x5C, 0x3E, 0x26, 0x87, 0x83, 0x13, 0x00
  )
  private[this] var vs_security : Array[Int] = _
  private[this] var vsSuperXevious = false
  private[this] var xeviousSelect = false
  private[this] var vsSecurityIndex = 0

  // CONSTRUCTOR
  checkSecurity()

  override def reset: Unit =
    xeviousSelect = false
    vsSecurityIndex = 0

  private def checkSecurity(): Unit =
    vs_security = null
    vsSuperXevious = false

    for game <- ines.game ; vsgame <- game.vsGame do
      vsgame.securityCode match
        case Some("RBIBASEBALL") =>
          vs_security = VS_SECURITY_RBIBASEBALL
        case Some("TKOBOXING") =>
          vs_security = VS_SECURITY_TKOBOXING
        case Some("SUPERXEVIOUS") =>
          vsSuperXevious = true
          vs_security = null
        case _ =>

  override def readCPU(address: Int): Int =
    if vs_security != null && (address & 0xFFFE) == 0x5E00 then
      if (address & 1) > 0 then
        val data = vs_security(vsSecurityIndex)
        vsSecurityIndex = (vsSecurityIndex + 1) & 0x1F
        data
      else
        vsSecurityIndex = 0
        0x5E
    else if vsSuperXevious && (address & 0xFC00) == 0x5400 then
      address match
        case 0x54FF => 0x05
        case 0x5678 => if xeviousSelect then 0x00 else 0x01
        case 0x578F => if xeviousSelect then 0xD1 else 0x89
        case 0x5567 =>
          xeviousSelect ^= true
          if xeviousSelect then 0x37 else 0x3E
    else
      super.readCPU(address)

  override def readPRG(address: Int): Int =
    if address < 0xA000 then
      readPRG_8K(address,reg(6))
    else
    if address < 0xC000 then
      readPRG_8K(address,reg(7))
    else
      ROM(ROM.length - 1)(address & 0x3FFF)

  override def writeCPU(address: Int, value: Int): Unit =
    super.writeCPU(address, value)

    if address >= 0x8000 && address < 0xA000 then
      if (address & 1) == 0 then
        selectedReg = value & 7
      else
        reg(selectedReg) = value & 0x3F

  override def readCHR(address: Int): Int =
    if address < 0x1000 then
      readCHR_2K(address,reg(address >> 11) >> 1)
    else
      readCHR_1K(address,reg((address >> 10) - 2))

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(reg)
    out.writeInt(selectedReg)
    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(reg,in)
    selectedReg = in.readInt()
    super.loadState(in)