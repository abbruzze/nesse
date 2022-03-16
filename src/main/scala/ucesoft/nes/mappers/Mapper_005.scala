package ucesoft.nes.mappers

import ucesoft.nes.cpu.CPU6502
import ucesoft.nes.{Cartridge, PPU}

class Mapper_005(ppu : PPU, ines:Cartridge.iNES, irqHandler: Boolean => Unit) extends Cartridge(ppu,ines,irqHandler):
  override val cartName : String = "MMC5"
  override val listenToAllAddresses = true

  private inline val _0000_1FFF = 7

  private inline val _6000_7FFF = 0
  private inline val _8000_9FFF = 1
  private inline val _A000_BFFF = 2
  private inline val _8000_BFFF = 2
  private inline val _C000_DFFF = 3
  private inline val _D000_FFFF = 4
  private inline val _8000_FFFF = 4
  private inline val _C000_FFFF = 4
  private inline val _E000_FFFF = 4

  private var prgMode = 3
  private var chrMode = 0
  private val prgRegs = Array(0,0,0,0,0)
  private val chrRegs = Array.ofDim[Int](12)
  private var lastWriteChrRegsLower = false
  private var chrHiReg = 0
  private val exRAM = Array.ofDim[Int](1024) // extended RAM
  private var exRAMMode = 0
  private var exRAMLastNTAddress,exRAMBlock = 0
  private val ntMapping = Array.ofDim[Int](4)
  private var fillModeTile,fillModeColor = 0
  private val romFlag = Array(true,true,true,true,true)
  private val ramProtect = Array(0,0)
  private var prgRAMWriteable = false
  private val dummyFillNT = Array.ofDim[Int](0)
  // TIMER
  private var timerRunning = false
  private var timerCounter = 0
  private var timerIRQFlag = false
  // MUL
  private var mulOp1, mulOp2 = 0xFF
  // SPRITE & RENDERING
  private var sprite8x8 = false
  private var renderingEnabled = false
  private var spriteMode = false
  // IRQ
  private var irqScanline = 0
  private var irqEnabled = false
  private var inFrame = false
  private var scanLineIRQPending = false
  private var scanline = 0
  // VERTICAL SPLIT
  private var ppuCycle = 0
  private var verticalSplitEnabled = false
  private var verticalSplitXTile = 0
  private var verticalSplitRight = false
  private var verticalSplitScroll = 0
  private var verticalOffset = 0
  private var verticalSplitBank = 0
  private var verticalSplitCHRRead = false
  /*
  private var matchCount = 0
  private var idleCount = 0
  private var ppuIsReading = false
  private var lastAddress = -1
  */

  // Constructor
  customNTHandling = true
  initBanks()

  private def initBanks(): Unit =
    prgMode = 3
    chrMode = 0
    var b = 0
    java.util.Arrays.fill(prgRegs,0x7F)
    java.util.Arrays.fill(romFlag,true)
    /*
    val bank8K = (ROM.length << 1) - 1
    while b < prgRegs.length do
      prgRegs(b) = 0x7F //bank8K
      romFlag(b) = true
      b += 1
    */

  override def reset: Unit =
    super.reset
    initBanks()

  override protected def initStaticRam() : Array[Int] = Array.ofDim[Int](0x40000)

  // ========================= Scanline Detection and Scanline IRQ ===========================

  override def ppuClock(ppuClockCycle: Int,rasterLine:Int): Unit =
    ppuCycle = ppuClockCycle

    if rasterLine == 240 && ppuClockCycle == 0 then
      inFrame = false
      //scanline = 0

    if renderingEnabled && rasterLine < 240 then
      ppuClockCycle match
        case 3 =>
          rasterLine match
            case 0 =>
              scanLineIRQPending = false
            case 1 =>
              inFrame = true
            case _ =>
          scanline = rasterLine
          if scanline != 0 && scanline == irqScanline then
            scanLineIRQPending = true
          checkIRQ()
        case 256 =>
          spriteMode = true
        case 320 =>
          spriteMode = false
          if rasterLine == -1 then
            verticalOffset = verticalSplitScroll
            if verticalOffset >= 240 then
              verticalOffset -= 16
          else if rasterLine < 240 then
            verticalOffset += 1
          if verticalOffset >= 240 then
            verticalOffset -= 240
        case _ =>

  override def cpuClock(): Unit =
    /*
    if ppuIsReading then
      idleCount = 0
    else
      idleCount += 1
      if idleCount == 3 then
        inFrame = false
        lastAddress = -1
    ppuIsReading = false
    */
    if timerRunning then
      timerCounter = (timerCounter - 1) & 0xFFFF
      if timerCounter == 0 then
        timerRunning = false
        timerIRQFlag = true
        checkIRQ()

  override protected def readCPU(address:Int) : Int =
    /*
    if address == 0xFFFA || address == 0xFFFB then
      inFrame = false
      scanline = 0
      scanLineIRQPending = false
      checkIRQ()
      lastAddress = -1
    */

    if address >= 0x5000 then
      if address < 0x5C00 then
        openbus = readRegisters(address)
      else if address < 0x6000 then
        openbus = readEXRAM(address)
      else
        openbus = read(address)

    openbus

  override protected def writeCPU(address: Int, value: Int): Unit =
    address match
      case 0x2000 =>
        sprite8x8 = (value & 0x20) == 0
      case 0x2001 =>
        if (value & 0x18) == 0 then
          inFrame = false
          //lastAddress = -1
          renderingEnabled = false
        else
          renderingEnabled = true
      case _ =>
        if address >= 0x5000 then
          if address < 0x5C00 then
            writeRegisters(address,value)
          else if address < 0x6000 then
            writeEXRAM(address,value)
          else
            write(address,value)

/*
  override protected def readPPU(address: Int, readOnly: Boolean): Int =
    if address >= 0x2000 && address <= 0x2FFF && address == lastAddress then
        matchCount += 1
        if matchCount == 2 then
          if !inFrame then
            inFrame = true
            scanline = 0
            scanLineIRQPending = false
            checkIRQ()
          else
            scanline += 1
            if scanline == irqScanline then
              scanLineIRQPending = true
              checkIRQ()
            if scanline == 240 then
              inFrame = false
              scanline = 0
              scanLineIRQPending = false
              checkIRQ()
    else
      matchCount = 0

    lastAddress = address
    ppuIsReading = true

    super.readPPU(address, readOnly)
*/

  // =========================================================================================

  inline private def checkIRQ(): Unit =
    irqHandler(timerIRQFlag || (irqEnabled && scanLineIRQPending))

  inline private def readEXRAM(address:Int): Int =
    exRAMMode match
      case 0|1 => openbus
      case _ => exRAM(address & 0x3FF)

  inline private def writeEXRAM(address:Int,value:Int):Unit =
    exRAMMode match
      case 0|1 =>
        val v = if inFrame then value else 0
        exRAM(address & 0x3FF) = v
      case 2 =>
        exRAM(address & 0x3FF) = value
      case 3 => // read-only

  inline private def readRegisters(address:Int): Int =
    address match
      case 0x5204 =>
        var r = 0
        if inFrame then r |= 0x40
        if scanLineIRQPending then r |= 0x80
        scanLineIRQPending = false
        checkIRQ()
        r
      case 0x5205 =>
        (mulOp1 * mulOp2) & 0xFF
      case 0x5206 =>
        ((mulOp1 * mulOp2) >> 8) & 0xFF
      case 0x5209 =>
        val read = if timerRunning then 0x00
        else if timerIRQFlag then 0x80 else 0x00

        timerIRQFlag = false
        checkIRQ()
        read
      case _ =>
        openbus

  inline private def writeRegisters(address:Int,value:Int): Unit =
    //println(s"MMC5: write ${address.toHexString.toUpperCase} = $value")
    if address < 0x5016 then { /*TODO: MMC5 audio */ }
    else if address == 0x5100 then
      prgMode = value & 3
    else if address == 0x5101 then
      chrMode = value & 3
    else if address == 0x5102 then
      ramProtect(0) = value & 3
      prgRAMWriteable = ramProtect(0) == 0x2 && ramProtect(1) == 0x1
    else if address == 0x5103 then
      ramProtect(1) = value & 3
      prgRAMWriteable = ramProtect(0) == 0x2 && ramProtect(1) == 0x1
    else if address == 0x5104 then
      exRAMMode = value & 3
    else if address == 0x5105 then
      setMirroringMode(value)
    else if address == 0x5106 then
      fillModeTile = value
    else if address == 0x5107 then
      val c = value & 3
      fillModeColor = c | c << 2 | c << 4 | c << 6
    else if address >= 0x5113 && address < 0x5118 then
      val index = address - 0x5113
      prgRegs(index) = value & 0x7F
      romFlag(index) = (value & 0x80) > 0
    else if address >= 0x5120 && address < 0x512C then
      val i = address & 0x0F
      chrRegs(i) = value
      lastWriteChrRegsLower = i < 8
    else if address == 0x5130 then
      chrHiReg = (value & 3) << 8
    else if address == 0x5200 then
      verticalSplitEnabled = (value & 0x80) > 0
      verticalSplitXTile = value & 0x1F
      verticalSplitRight = (value & 0x40) > 0
    else if address == 0x5201 then
      verticalSplitScroll = value
    else if address == 0x5202 then
      verticalSplitBank = value
    else if address == 0x5203 then
      irqScanline = value
    else if address == 0x5204 then
      irqEnabled = (value & 0x80) > 0
    else if address == 0x5205 then
      mulOp1 = value
    else if address == 0x5206 then
      mulOp2 = value
    else if address == 0x5209 then
      timerCounter = (timerCounter & 0xFF00) | value
      timerRunning = true
    else if address == 0x520A then
      timerCounter = (timerCounter & 0x00FF) | (value << 8)

  private def setMirroringMode(_value:Int): Unit =
    var value = _value
    var c = 0
    while c < 4 do
      val nt = value & 3 match
        case 0 => ppu.pt0
        case 1 => ppu.pt1
        case 2 => exRAM
        case 3 => dummyFillNT

      c match
        case 0 =>
          ppu.nt_2000 = nt
        case 1 =>
          ppu.nt_2400 = nt
        case 2 =>
          ppu.nt_2800 = nt
        case 3 =>
          ppu.nt_2C00 = nt

      value >>= 2
      c += 1

  inline private def decodeNT(index:Int): Array[Int] =
    index match
      case 0 => ppu.nt_2000
      case 1 => ppu.nt_2400
      case 2 => ppu.nt_2800
      case 3 => ppu.nt_2C00

  inline private def ppucycle2tile(c:Int): Int = (c - 1) >> 3

  /**
   *
    1. PPU fetches an NT byte; that is, (addr >= 0x2000 && (addr & 0x3ff) < 0x3c0)
    Return the NT fetched byte as requested from whatever nametable is in context. Let N = addr & 0x3ff; that will be your index into ExRAM for the next fetch.

    2. PPU fetches the corresponding AT byte.
    Do not return anything from normal PPU space here. Instead, return the two top bits of exram[N] as the attribute (they will need to be shifted into the correct bit position).
    Let B = exram[N] & 0x3F; this will give you the block to get pattern tables from.

    3. PPU fetches the low pattern byte (first bitplane).
    The bottom 12 bits of the address for this should be passed through as is from the PPU. The next 6 bits are B. The top two bits are from the register at 0x5130.
    This gives a total of 20 bits of address into a 1MiB CHR ROM; remove top bits for smaller CHR ROMs as appropriate.

    4. PPU fetches the high pattern byte (second bitplane)
    Do exactly as in #3. PPU A3 will be high this time, where it was low last time.
   *
   */
  override protected def readNT(address: Int, index: Int): Int =
    verticalSplitCHRRead = false
    if verticalSplitEnabled && exRAMMode < 2 then
      val vr = readVerticalSplit(address)
      if vr != -1 then return vr

    val nt = decodeNT(index)

    if nt != dummyFillNT then
      if exRAMMode == 1 && !spriteMode then
        val adr = address & 0x3FF
        if adr < 0x3C0 then
          exRAMLastNTAddress = adr
          nt(exRAMLastNTAddress)
        else
          val value = exRAM(exRAMLastNTAddress)
          exRAMBlock = value & 0x3F
          val pal = value >> 6
          pal | pal << 2 | pal << 4 | pal << 6
      else
        nt(address & 0x3FF)
    else if (address & 0x3FF) < 0x3C0 then fillModeTile
    else fillModeColor

  inline private def readVerticalSplit(address:Int): Int =
    val cycleTile = ppucycle2tile(ppuCycle) + 2
    if cycleTile < 32 &&
      ((verticalSplitRight && cycleTile >= verticalSplitXTile) ||
        (!verticalSplitRight && cycleTile < verticalSplitXTile)) then
          verticalSplitCHRRead = true
          val adr = address & 0x3FF
          if adr >= 0x3C0 then
            exRAM((0x3C0 + (cycleTile >> 2) + ((verticalOffset >> 5) << 3)) & 0x3FF)
          else
            exRAM((cycleTile + ((verticalOffset >> 3) << 5)) & 0x3FF)
    else
      -1

  override protected def writeNT(address: Int, index: Int, value: Int): Unit =
    val nt = decodeNT(index)

    if nt != dummyFillNT then
      nt(address & 0x3FF) = value

  inline private def readBanked8RAM(address:Int,bank:Int): Int =
    SRAM(bank << 13 | (address & 0x1FFF))

  inline private def writeBanked8RAM(address:Int,bank:Int,value:Int): Unit =
    if prgRAMWriteable then
      SRAM(bank << 13 | (address & 0x1FFF)) = value

  inline private def readBanked16RAM(address:Int,bank:Int): Int =
    SRAM(bank << 14 | (address & 0x3FFF))

  inline private def writeBanked16RAM(address:Int,bank:Int,value:Int): Unit =
    if prgRAMWriteable then
      SRAM(bank << 14 | (address & 0x3FFF)) = value

  private def read(address: Int): Int =
    prgMode match
      case 0 =>
        if address < 0x8000 then readBanked8RAM(address,prgRegs(_6000_7FFF))     // RAM 8K
        else readPRG_32K(address,prgRegs(_8000_FFFF) >> 2)                       // ROM 32K
      case 1 =>
        if address < 0x8000 then readBanked8RAM(address,prgRegs(_6000_7FFF))     // RAM 8K
        else if address < 0xC000 then
          if romFlag(_8000_BFFF) then
            ROM(prgRegs(_8000_BFFF) >> 1)(address & 0x3FFF)                      // ROM 16K
          else readBanked16RAM(address,prgRegs(_8000_BFFF) >> 1)                 // RAM 16K
        else ROM(prgRegs(_C000_FFFF) >> 1)(address & 0x3FFF)                     // ROM 16K
      case 2 =>
        if address < 0x8000 then readBanked8RAM(address,prgRegs(_6000_7FFF))     // RAM 8K
        else if address < 0xC000 then
          if romFlag(_8000_BFFF) then
            ROM(prgRegs(_8000_BFFF) >> 1)(address & 0x3FFF)                      // ROM 16K
          else readBanked16RAM(address,prgRegs(_8000_BFFF) >> 1)                 // RAM 16K
        else if address < 0xE000 then
          if romFlag(_C000_DFFF) then readPRG_8K(address,prgRegs(_C000_DFFF))    // ROM 8K
          else readBanked8RAM(address,prgRegs(_C000_DFFF))                       // RAM 8K
        else readPRG_8K(address,prgRegs(_E000_FFFF))                             // ROM 8K
      case 3 =>
        if address < 0x8000 then readBanked8RAM(address,prgRegs(_6000_7FFF))     // RAM 8K
        else if address < 0xA000 then
          if romFlag(_8000_9FFF) then readPRG_8K(address,prgRegs(_8000_9FFF))    // ROM 8K
          else readBanked8RAM(address,prgRegs(_8000_9FFF))                       // RAM 8K
        else if address < 0xC000 then
          if romFlag(_A000_BFFF) then readPRG_8K(address,prgRegs(_A000_BFFF))    // ROM 8K
          else readBanked8RAM(address,prgRegs(_A000_BFFF))                       // RAM 8K
        else if address < 0xE000 then
          if romFlag(_C000_DFFF) then readPRG_8K(address,prgRegs(_C000_DFFF))    // ROM 8K
          else readBanked8RAM(address,prgRegs(_C000_DFFF))                       // RAM 8K
        else readPRG_8K(address,prgRegs(_E000_FFFF))                             // ROM 8K

  private def write(address:Int,value:Int):Unit =
    prgMode match
      case 0 =>
        if address < 0x8000 then writeBanked8RAM(address,prgRegs(_6000_7FFF),value)  // RAM 8K
      case 1 =>
        if address < 0x8000 then writeBanked8RAM(address,prgRegs(_6000_7FFF),value)  // RAM 8K
        else if address < 0xC000 then
          if !romFlag(_8000_BFFF) then
            writeBanked16RAM(address,prgRegs(_8000_BFFF) >> 1,value)                 // RAM 16K
      case 2 =>
        if address < 0x8000 then writeBanked8RAM(address,prgRegs(_6000_7FFF),value)  // RAM 8K
        else if address < 0xA000 then
          if !romFlag(_8000_BFFF) then
            writeBanked16RAM(address,prgRegs(_8000_BFFF) >> 1,value)                 // RAM 16K
        else if address < 0xE000 then
            if !romFlag(_C000_DFFF) then
              writeBanked8RAM(address,prgRegs(_C000_DFFF),value)                     // RAM 8K
      case 3 =>
        if address < 0x8000 then writeBanked8RAM(address,prgRegs(_6000_7FFF),value)  // RAM 8K
        else if address < 0xA000 then
          if !romFlag(_8000_9FFF) then
            writeBanked8RAM(address,prgRegs(_8000_9FFF),value)                       // RAM 8K
        else if address < 0xC000 then
          if !romFlag(_A000_BFFF) then
            writeBanked8RAM(address,prgRegs(_A000_BFFF),value)                       // RAM 8K
        else if address < 0xE000 then
          if !romFlag(_C000_DFFF) then
            writeBanked8RAM(address,prgRegs(_C000_DFFF),value)                       // RAM 8K

  override def readCHR(address: Int): Int =
    if verticalSplitCHRRead && !spriteMode then
      val scrolledAdr = (address & 0xFF8) | (verticalOffset & 7)
      readCHR_4K(scrolledAdr,verticalSplitBank)
    else if exRAMMode == 1 && !spriteMode then
      readCHR_4K(address,exRAMBlock | chrHiReg >> 2)
    else if sprite8x8 then
        readCHR8x8(address)
    else if ppu.isIOInProgress then
      if lastWriteChrRegsLower then
        readCHR8x8(address)
      else
        readCHR8x16(address)
    else if spriteMode then
      readCHR8x8(address)
    else
      readCHR8x16(address)

  inline private def readCHR8x8(address:Int): Int =
    chrMode match
      case 0 =>
        CHR(chrRegs(_0000_1FFF) | chrHiReg)(address & 0x1FFF)
      case 1 =>
        readCHR_4K(address,chrRegs(3 + ((address >> 12) << 2)))
      case 2 =>
        readCHR_2K(address,chrRegs(1 + ((address >> 11) << 1)) | chrHiReg)
      case 3 =>
        readCHR_1K(address,chrRegs(address >> 10) | chrHiReg)

  inline private def readCHR8x16(address:Int): Int =
    chrMode match
      case 0 =>
        CHR(chrRegs(11))(address & 0x1FFF)
      case 1 =>
        readCHR_4K(address,chrRegs(11))
      case 2 =>
        if (address & 0x0FFF) < 0x800 then
          readCHR_2K(address,chrRegs(9) | chrHiReg)
        else
          readCHR_2K(address,chrRegs(11) | chrHiReg)
      case 3 =>
        readCHR_1K(address,chrRegs(8 + ((address & 0xFFF) >> 10)) | chrHiReg)

  /*
  override protected def writeCHR(address:Int,value:Int) : Unit =
    if sprite8x8 then
      writeCHR8x8(address,value)
    else if spriteMode then
      readCHR8x8(address)
    else
      writeCHR8x16(address,value)

  inline private def writeCHR8x8(address:Int,value:Int): Unit =
    chrMode match
      case 0 =>
        CHR(chrRegs(_0000_1FFF) | chrHiReg)(address & 0x1FFF) = value
      case 1 =>
        writeCHR_4K(address,chrRegs(3 + ((address >> 12) << 2)),value)
      case 2 =>
        writeCHR_2K(address,chrRegs(1 + ((address >> 11) << 1)) | chrHiReg,value)
      case 3 =>
        writeCHR_1K(address,chrRegs(address >> 10) | chrHiReg,value)

  inline private def writeCHR8x16(address:Int,value:Int): Unit =
    chrMode match
      case 0 =>
        CHR(chrRegs(11))(address & 0x1FFF) = value
      case 1 =>
        writeCHR_4K(address,chrRegs(11),value)
      case 2 =>
        if (address & 0x0FFF) < 0x800 then
          writeCHR_2K(address,chrRegs(9) | chrHiReg,value)
        else
          writeCHR_2K(address,chrRegs(11) | chrHiReg,value)
      case 3 =>
        writeCHR_1K(address,chrRegs(8 + ((address & 0xFFF) >> 10)) | chrHiReg,value)
  */