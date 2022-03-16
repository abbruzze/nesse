package ucesoft.nes

import ucesoft.nes.cpu.{CPU6502, Memory}

import java.io.{ObjectInputStream, ObjectOutputStream}
import ucesoft.nes.Cartridge.Overscan
import ucesoft.nes.controller.InputDevice

import java.util.Properties

object PPU {
  inline val DISPLAY_WIDTH = 32 * 8
  inline val DISPLAY_HEIGHT = 30 * 8

  enum PPUType:
    case _2C02,_2C03,_2C03B,_2C04_0001,_2C04_0002,_2C04_0003,_2C04_0004,_2C05

  val DEFAULT_OVERSCAN = Overscan(0,DISPLAY_HEIGHT,0,DISPLAY_WIDTH)

  private val SCANLINE_COLORS : Array[Int] = {
    val colors = Array.ofDim[Int](256)
    for(c <- colors.indices) colors(c) = (c * 0.80).toInt
    colors
  }

  case class PPUTableImage(pixels:Array[Int], width:Int, height:Int)
}

class PPU(nmiHandler:() => Unit) extends NESComponent with Memory {
  import PPU.*

  override val name: String = "PPUMemory"
  override val componentID: String = "PPU"
  override val componentType: NESComponentType = NESComponentType.PPU

  private[this] var cartridge : Cartridge = _

  private[this] inline val PRE_RENDER_LINE = -1
  private[this] inline val FIRST_VBLANK_LINE = 240
  private[this] var LAST_VBLANK_LINE = 260

  // status vblank bit in status register
  private[this] var vblankFlag = false
  // vertical blank mode
  private[this] var vblank = false
  // background shifters
  private[this] var shifterBGLow, shifterBGHi = 0
  private[this] var shifterAttrLow, shifterAttrHi = 0
  private[this] var nextTileBGLow, nextTileBGHi = 0
  private[this] var nextTileAttr = 0
  private[this] var nextNtByte = 0

  // display
  private[this] var pixels : Array[Int] = _
  private[this] var display : Display = _
  private[this] var overscan : Cartridge.Overscan = DEFAULT_OVERSCAN
  private[this] var ignoreOverscan = false

  private[this] var ppuType : PPUType = PPUType._2C02
  private[this] var ppuVSStatus = 0
  private[this] var _2000_2001_exchanged = false
  
  private[this] var i_o_inProgress = false

  private[this] final val PALETTE_POWER_ON = Array( // power-on values
    0x09, 0x01, 0x00, 0x01, 0x00, 0x02, 0x02, 0x0D,
    0x08, 0x10, 0x08, 0x24, 0x00, 0x00, 0x04, 0x2C,
    0x09, 0x01, 0x34, 0x03, 0x00, 0x04, 0x00, 0x14,
    0x08, 0x3A, 0x00, 0x02, 0x00, 0x20, 0x2C, 0x08
  )
  private[this] var _palette : Palette = Palette.getPaletteFor(ppuType)
  private[this] var paletteRGB : Array[Array[Int]] = _palette.palette


  /**
   * Located at 0x3F00 - 0x3F1F
   */
  final val palette = Array.ofDim[Int](32)

  /**
   * Pattern tables: 0x2000 - 0x3000
   */
  val pt0 : Array[Int] = Array.ofDim[Int](0x400)
  val pt1 : Array[Int] = Array.ofDim[Int](0x400)
  var nt_2000 : Array[Int] = pt0
  var nt_2400 : Array[Int] = pt1
  var nt_2800 : Array[Int] = nt_2000
  var nt_2C00 : Array[Int] = nt_2400
  // ============== Status Registers =====================================================
  private[this] inline val STATUS_VBLANK_MASK        = 0x80
  private[this] inline val STATUS_SPRITE0_HIT_MASK   = 0x40
  private[this] inline val STATUS_SPRITE_OVER_MASK   = 0x20
  private[this] inline val STATUS_01234_MASK         = 0x01 | 0x02 | 0x04 | 0x8 | 0x10
  // ============== Control Registers ====================================================
  private[this] inline val CTRL1_VBLANK_MASK         = 0x80
  private[this] inline val CTRL1_SPRITE_SIZE_MASK    = 0x20
  private[this] inline val CTRL1_BACKGROUND_PTA_MASK = 0x10
  private[this] inline val CTRL1_SPRITE_PTA_MASK     = 0x08
  private[this] inline val CTRL1_VRAM_INC_MASK       = 0x04
  private[this] inline val CTRL1_NT_SCROLL_MASK      = 0x03
  // ============= Internal Registers ====================================================
  /**
   * yyy NN YYYYY XXXXX
   * ||| || ||||| +++++-- coarse X scroll
   * ||| || +++++-------- coarse Y scroll
   * ||| ++-------------- nametable select
   * +++----------------- fine Y scroll
   */
  private[this] var v = 0 // Current VRAM address (15 bits)
  private[this] var t = 0 // Temporary VRAM address (15 bits); can also be thought of as the address of the top left onscreen tile
  private[this] var finex = 0 // Fine X scroll (3 bits)
  private[this] var w = 0 // First or second write toggle (1 bit)
  /**
   * Address 0x2000
   *
   * Bit7 Execute NMI on VBlank (0=Disabled, 1=Enabled)
   * Bit6 PPU Master/Slave Selection (0=Master, 1=Slave) (Not used in NES)
   * Bit5 Sprite Size (0=8x8, 1=8x16)
   * Bit4 Pattern Table Address Background (0=VRAM 0000h, 1=VRAM 1000h)
   * Bit3 Pattern Table Address 8x8 Sprites (0=VRAM 0000h, 1=VRAM 1000h)
   * Bit2 Port 2007h VRAM Address Increment (0=Increment by 1, 1=Increment by 32)
   * Bit1-0 Name Table Scroll Address (0-3=VRAM 2000h,2400h,2800h,2C00h)
   * (That is, Bit0=Horizontal Scroll by 256, Bit1=Vertical Scroll by 240)
   */
  private[this] var ctrl1 = 0
  private[this] inline val CTRL2_COLOR_EMPH_MASK     = 0x80 | 0x40 | 0x20
  private[this] inline val CTRL2_SPRITE_ON_MASK      = 0x10
  private[this] inline val CTRL2_BACKGROUND_ON_MASK  = 0x08
  private[this] inline val CTRL2_SPRITE_CLIP_MASK    = 0x04
  private[this] inline val CTRL2_BACKGROUND_CLIP_MASK= 0x02
  private[this] inline val CTRL2_MONOCHROME_MODE_MASK= 0x01
  /**
   * Address 0x2001
   *
   * Bit7-5 Color Emphasis (0=Normal, 1-7=Emphasis) (see Palettes chapter)
   * Bit4 Sprite Visibility (0=Not displayed, 1=Displayed)
   * Bit3 Background Visibility (0=Not displayed, 1=Displayed)
   * Bit2 Sprite Clipping (0=Hide in left 8-pixel column, 1=No clipping)
   * Bit1 Background Clipping (0=Hide in left 8-pixel column, 1=No clipping)
   * Bit0 Monochrome Mode (0=Color, 1=Monochrome)
   */
  private[this] var ctrl2 = 0

  private[this] var rasterLine = 0
  private[this] var cycle = 0
  private[this] var oddFrame = false

  // ============== OAM ==================================================================
  private[this] inline val SPRITE_Y = 0
  private[this] inline val SPRITE_TILE = 1
  private[this] inline val SPRITE_ATTR = 2
  private[this] inline val SPRITE_X = 3
  private[this] inline val SPRITE_H_FLIP_MASK = 0x40
  private[this] inline val SPRITE_V_FLIP_MASK = 0x80
  private[this] inline val SPRITE_PRIORITY_MASK = 0x20
  private[this] inline val SPRITE_PALETTE_MASK = 0x7

  private[this] final val OAM = Array.fill[Int](256)(0xFF) // 64 x 4
  private[this] final val secondOAM = Array.fill[Int](32)(0xFF) // 8 x 4
  private[this] var oamAddress = 0
  // sprite 0 hit flag
  private[this] var sprite0HitFlag = false
  // sprite overflow flag
  private[this] var spriteOverflowFlag = false
  private[this] final val shifterSpriteLow, shifterSpriteHi = Array.ofDim[Int](8)
  private[this] final val spriteAttrLatch, spriteXLatch = Array.ofDim[Int](8)
  private[this] var evalSprite0OnLine,sprite0Online = false
  private[this] var spriteAddressLow = 0
  private var oamPtr, secOamPtr,oamValue = 0

  private enum OAMState:
    case FIND,COPY,FIND_OVERFLOW,READ_OVERFLOW,_64_EVALUATED

  private var oamState = OAMState.FIND

  private[this] var lastDataOnBus = 0

  private[this] var scrollX, scrollY = 0

  private[this] var frameListener : () => Unit = _

  /**
   * When reading while the VRAM address is in the range 0-$3EFF (i.e., before the palettes), the read will return the contents of an internal read buffer.
   * This internal buffer is updated only when reading PPUDATA, and so is preserved across frames. After the CPU reads and gets the contents of the internal buffer,
   * the PPU will immediately update the internal buffer with the byte at the current VRAM address.
   */
  private[this] var ppuDataReadBuffer = 0

  // Display Cache
  private[this] var minModX,maxModX,minModY,maxModY = 0
  private[this] var pixelMod = false

  // Scanline effect
  private[this] var applyScanLineEffect = false
  
  // Zapper, Rob
  private[this] var optical : InputDevice = _

  // ======================================================================================
  override def getProperties: Properties =
    val p = super.getProperties
    p.setProperty("Vertical Blank",vblank.toString)
    p.setProperty("v",s"0x${v.toHexString}")
    p.setProperty("t",s"0x${t.toHexString}")
    p.setProperty("finex",s"0x${finex.toHexString}")
    p.setProperty("w",w.toString)
    p.setProperty("Control register 1",s"0x${ctrl1.toHexString}")
    p.setProperty("Control register 2",s"0x${ctrl2.toHexString}")
    p.setProperty("Raster line",rasterLine.toString)
    p.setProperty("Raster cycle",cycle.toString)
    p.setProperty("OAM address",s"0x${oamAddress.toHexString}")
    p.setProperty("Sprite 0 hit flag",sprite0HitFlag.toString)
    p.setProperty("Sprite overflow flag",spriteOverflowFlag.toString)
    p.setProperty("OAM state",oamState.toString)
    p

  def setPPUType(ppuType:PPUType):Unit =
    this.ppuType = ppuType
    _palette = Palette.getPaletteFor(ppuType)
    paletteRGB = _palette.palette
    _2000_2001_exchanged = ppuType == PPUType._2C05
    
  def getPalette: Palette = _palette

  def enableScanlineEffect(enabled:Boolean): Unit =
    applyScanLineEffect = enabled
  
  def setOpticalDevice(optical:InputDevice): Unit = this.optical = optical
  
  def setFrameListener(fl:() => Unit): Unit = frameListener = fl

  def setIgnoreOverscan(ignore:Boolean): Unit =
    ignoreOverscan = ignore
    if !ignoreOverscan then
      overscan = cartridge.ines.tv.overScan
    else
      overscan = DEFAULT_OVERSCAN

  def setCartridge(cart:Cartridge) : Unit =
    cartridge = cart
    LAST_VBLANK_LINE = cart.ines.tv.totalScanLines - 2
    if !ignoreOverscan then
      overscan = cart.ines.tv.overScan

    cart.ines.game match
      case Some(game) =>
        game.vsGame match
          case Some(vsGame) =>
            setPPUType(vsGame.ppuType)
            ppuVSStatus = vsGame.ppuStatusCode.getOrElse(0)
          case None =>
            setPPUType(PPUType._2C02)
      case None =>
        setPPUType(PPUType._2C02)

  def getCartridge(): Cartridge = cartridge

  def setDisplay(display:Display) : Unit =
    this.display = display
    pixels = display.displayMem

  def setOverscan(over:Overscan) : Unit = overscan = over

  override def init: Unit = {}
  override def reset: Unit =
    vblankFlag = false
    vblank = false
    shifterBGLow = 0
    shifterBGHi = 0
    shifterAttrLow = 0
    shifterAttrHi = 0
    nextTileBGLow = 0
    nextTileBGHi = 0
    nextTileAttr = 0
    nextNtByte = 0
    v = 0
    t = 0
    finex = 0
    w = 0
    ctrl1 = 0
    ctrl2 = 0
    rasterLine = 0
    cycle = 0
    oddFrame = false
    java.util.Arrays.fill(OAM,0xFF)
    java.util.Arrays.fill(secondOAM,0xFF)
    oamAddress = 0
    oamState = OAMState.FIND
    sprite0HitFlag = false
    spriteOverflowFlag = false
    sprite0Online = false
    evalSprite0OnLine = false
    spriteAddressLow = 0
    lastDataOnBus = 0
    ppuDataReadBuffer = 0
    minModX = 0
    maxModX = 0
    minModY = 0
    maxModY = 0
    pixelMod = false
    scrollX = 0
    scrollY = 0

  override def hardReset: Unit =
    reset
    optical = null
    System.arraycopy(PALETTE_POWER_ON,0,palette,0,32)
    java.util.Arrays.fill(pt0,0)
    java.util.Arrays.fill(pt1,0)

  def getCycle : Int = cycle
  def getRasterLine : Int = rasterLine
  def getAddress : Int = v
  def isVBlank : Boolean = vblank
  def isVBlankFlag : Boolean = vblankFlag
  def isBGOn : Boolean = (ctrl2 & CTRL2_BACKGROUND_ON_MASK) > 0
  def isSpriteOn : Boolean = (ctrl2 & CTRL2_SPRITE_ON_MASK) > 0
  
  final def isIOInProgress : Boolean = i_o_inProgress

  final override def read(address: Int, chipID: ChipID): Int =
    lastDataOnBus = address match
      case 2 =>
        // PPUSTATUS: <VSO- ----> vblank (V), sprite 0 hit (S), sprite overflow (O); read resets write pair for $2005/$2006
        var status = (if vblankFlag then STATUS_VBLANK_MASK else 0) |
                     (if sprite0HitFlag then STATUS_SPRITE0_HIT_MASK else 0) |
                     (if spriteOverflowFlag then STATUS_SPRITE_OVER_MASK else 0)

        vblankFlag = false
        w = 0
        if _2000_2001_exchanged then
          status |= ppuVSStatus
        else
          status |= lastDataOnBus & STATUS_01234_MASK
        status
      case 4 =>
        // OAMDATA: <dddd dddd> OAM data read/write
        if isDisplayOn && cycle > 0 && cycle < 65 && isRendering then 0xFF // secondary OAM cleaning
        else
          OAM(oamAddress)
        // TODO: check if display is on ...
      case 7 =>
        // PPUDATA: <dddd dddd> PPU data read/write
        /* When reading while the VRAM address is in the range 0-$3EFF (i.e., before the palettes),
         * the read will return the contents of an internal read buffer
        */
        i_o_inProgress = true
        val r = if v < 0x3F00 then
          val ret = ppuDataReadBuffer
          ppuDataReadBuffer = cartridge.read(v & 0x3FFF,ChipID.PPU)
          ret
        else
          val ret = cartridge.read(v & 0x3FFF,ChipID.PPU)
          ppuDataReadBuffer = cartridge.read((v & 0x3FFF) - 0x1000,ChipID.PPU) // not clear
          ret
        i_o_inProgress = false

        // check address increment
        if !isDisplayOn || (!isRendering && !isPreRenderLine) then
          incAddress()
        else
          incH()
          incV()

        r
      case _ =>
        lastDataOnBus

    lastDataOnBus

  final override def write(_address: Int, value: Int, chipID: ChipID): Unit =
    var address = _address & 7
    lastDataOnBus = value

    if _2000_2001_exchanged then
      if address == 0 then address = 1
      else if address == 1 then address = 0

    address match
      case 0 =>
        //Log.debug(s"PPU:CTRL1 = ${value.toHexString}")
        // PPUCTRL: <VPHB SINN>	NMI enable (V), PPU master/slave (P), sprite height (H), background tile select (B), sprite tile select (S), increment mode (I), nametable select (NN)
        val preV = ctrl1 & CTRL1_VBLANK_MASK
        ctrl1 = value
        // If the PPU is currently in vertical blank, and the PPUSTATUS ($2002) vblank flag is still set (1), changing the NMI flag in bit 7 of $2000 from 0 to 1 will immediately generate an NMI
        if preV == 0 && (ctrl1 & CTRL1_VBLANK_MASK) > 0 && vblank && vblankFlag then triggerNMI()

        // Update t with bit 0-1 of value (NN: nametable select)
        // t: ...GH.. ........ <- d: ......GH
        t &= ~0xC00 // clears t.GH
        t |= (value & 0x03) << 10
      case 1 =>
        //Log.debug(s"PPU:CTRL2 = ${value.toHexString}")
        // PPUMASK: <BGRs bMmG> color emphasis (BGR), sprite enable (s), background enable (b), sprite left column enable (M), background left column enable (m), greyscale (G)
        ctrl2 = value
      case 3 =>
        //Log.debug(s"PPU:OAMADDR = ${value.toHexString}")
        // OAMADDR: <aaaa aaaa> OAM read/write address
        oamAddress = value & 0xFF
      case 4 =>
        //Log.debug(s"PPU:OAMDATA = ${value.toHexString}")
        // OAMDATA: <dddd dddd> OAM data read/write
        // byte 2 has bits 2,3 and 4 unimplemented
        val data = if (oamAddress & 3) == 2 then value & 0xE3 else value
        OAM(oamAddress) = data
        oamAddress = (oamAddress + 1) & 0xFF
      case 5 =>
        //Log.debug(s"PPU:PPUSCROLL = ${value.toHexString}")
        // PPUSCROLL: <xxxx xxxx> fine scroll position (two writes: X scroll, Y scroll)
        if w == 0 then
          scrollX = value
          // t: ....... ...ABCDE <- d: ABCDE...
          t &= ~0x1F // clears t.ABCDE
          t |= (value >> 3) & 0x1F
          // x: FGH <- d: .....FGH
          finex = value & 0x07
        else
          scrollY = value
          // t: FGH..AB CDE..... <- d: ABCDEFGH
          t &= ~0xF3E0 // clears t.FGH, t.AB, t.CDE
          t |= (value & 0x07) << 12
          t |= (value & 0xF8) << 2

        w = (w + 1) & 1
      case 6 =>
        //Log.debug(s"PPU:PPUADDR = ${value.toHexString}")
        // PPUADDR: <aaaa aaaa> PPU read/write address (two writes: most significant byte, least significant byte)
        if w == 0 then
          // t: .CDEFGH ........ <- d: ..CDEFGH
          // t: Z...... ........ <- 0 (bit Z is cleared)
          t &= 0x00FF
          t |= (value & 0x3F) << 8
        else
          // t: ....... ABCDEFGH <- d: ABCDEFGH
          t &= 0xFF00
          t |= value & 0xFF
          // v: <...all bits...> <- t: <...all bits...>
          v = t
          //Log.debug(s"v = ${v.toHexString}")

        w = (w + 1) & 1
      case 7 =>
        //Log.debug(s"PPU:PPUDATA = ${v.toHexString} <- ${value.toHexString}")
        // PPUDATA: <dddd dddd> PPU data read/write
        i_o_inProgress = true
        cartridge.write(v & 0x3FFF,value,ChipID.PPU)
        i_o_inProgress = false
        // check address increment
        if !isDisplayOn || (!isRendering && !isPreRenderLine) then
          incAddress()
        else // TODO: to be checked
          if (v & 0x7000) == 0x7000 then
            val yscroll = v & 0x3E0
            v &= 0xFFF
            yscroll match
              case 0x3A0 => v ^= 0xBA0
              case 0x3E0 => v ^= 0x3E0
              case _ => v += 0x20
          else v += 0x1000
      case _ =>


  inline private def isDisplayOn : Boolean = (ctrl2 & (CTRL2_BACKGROUND_ON_MASK | CTRL2_SPRITE_ON_MASK)) > 0

  inline private def triggerNMI() : Unit =
    if (ctrl1 & CTRL1_VBLANK_MASK) > 0 then nmiHandler()

  inline private def isRendering : Boolean = rasterLine < 240
  inline private def isPreRenderLine : Boolean = rasterLine == PRE_RENDER_LINE

  inline private def copyH() : Unit =
    // v: ....A.. ...BCDEF <- t: ....A.. ...BCDEF
    v &= 0xFBE0
    v |= t & 0x41F

  inline private def copyV() : Unit =
    // v: GHIA.BC DEF..... <- t: GHIA.BC DEF.....
    v &= 0x841F
    v |= t & 0x7BE0

  inline private def incH() : Unit =
    // increment horizontal part of v
    if (v & 0x001F) == 31 then // if coarse X == 31
      v &= ~0x001F  // coarse X = 0
      v ^= 0x0400   // switch horizontal nametable
    else
      v += 1        // increment coarse X

  inline private def incV() : Unit =
    // increment v to next row of tiles
    if (v & 0x7000) == 0x7000 then // reset the fine scroll bits and increment tile address to next row
      v &= ~0x7000
      var y = (v & 0x03E0) >> 5
      if y == 29 then // if row is 29 zero fine scroll and bump to next nametable
        y = 0
        v ^= 0x0800
      else
      if y == 31 then y = 0 // coarse Y = 0, nametable not switched
      else y += 1

      v = (v & ~0x03E0) | (y << 5)
    else v += 0x1000 // increment the fine scroll

  inline private def coarseX() : Int = v & 0x1F
  inline private def coarseY() : Int = (v >> 5) & 0x1F
  inline private def fineY() : Int = (v >> 12) & 7

  inline private def incAddress() : Unit =
    val raminc = if (ctrl1 & CTRL1_VRAM_INC_MASK) == 0 then 1 else 32
    v = (v + raminc) & 0x3FFF

  inline private def NTAddress() : Int = 0x2000 | (v & 0x0FFF)
  inline private def attributeAddress() : Int = 0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)

  inline private def loadBGShifters() : Unit =
    shifterBGLow = (shifterBGLow & 0xFF00) | nextTileBGLow
    shifterBGHi = (shifterBGHi & 0xFF00) | nextTileBGHi

    shifterAttrLow = (shifterAttrLow & 0xFF00) | (if (nextTileAttr & 1) > 0 then 0xFF else 0)
    shifterAttrHi = (shifterAttrHi & 0xFF00) | (if (nextTileAttr & 2) > 0 then 0xFF else 0)

  inline private def sprite_r(i:Int,pos:Int) : Int = secondOAM((i << 2) | pos)
  inline private def sprite_w(i:Int,pos:Int,value:Int) : Unit = secondOAM((i << 2) | pos) = value

  inline private def shiftBG() : Unit =
    if (ctrl2 & CTRL2_BACKGROUND_ON_MASK) > 0 then
      shifterBGLow <<= 1
      shifterBGHi <<= 1
      shifterAttrLow <<= 1
      shifterAttrHi <<= 1

    if (ctrl2 & CTRL2_SPRITE_ON_MASK) > 0 && cycle > 0 && cycle < 258 then
      var s = 0
      while s < 8 do
        if spriteXLatch(s) > 0 then spriteXLatch(s) -= 1
        else
          // check horizontal flip
          if (spriteAttrLatch(s) & SPRITE_H_FLIP_MASK) > 0 then
            shifterSpriteLow(s) >>= 1
            shifterSpriteHi(s) >>= 1
          else
            shifterSpriteLow(s) <<= 1
            shifterSpriteHi(s) <<= 1
        s += 1

  // ============================= Clock ================================================
  final def clock() : Unit = {
    cartridge.ppuClock(cycle,rasterLine)

    val displayON = isDisplayOn
    val preRenderLine = isPreRenderLine

    // visible area
    if rasterLine >= PRE_RENDER_LINE && rasterLine < FIRST_VBLANK_LINE then {
      // cycle 0, line 0: odd frame cycle skip
      if cycle == 0 && rasterLine == 0 && oddFrame && displayON then
        cycle = 1 // skip cycle 0

      // vblank flag clearing
      if cycle == 0 && preRenderLine then // with cycle == 1 vbl_clear_time.nes test passed, otherwise "VBL flag cleared too late"
        vblankFlag = false
        vblank = false
        sprite0HitFlag = false
        spriteOverflowFlag = false

      // ===================== BACKGROUND ================================================

      if displayON && ((cycle >= 2 && cycle < 258) || (cycle >= 321 && cycle < 338)) then {
        shiftBG()

        (cycle - 1) & 7 match {
          case 0 =>
            loadBGShifters()
            // read NT byte
            nextNtByte = cartridge.read(NTAddress(),ChipID.PPU)
          case 2 =>
            // read attribute
            nextTileAttr = cartridge.read(attributeAddress(),ChipID.PPU)

            if (coarseY() & 2) > 0 then nextTileAttr >>= 4
            if (coarseX() & 2) > 0 then nextTileAttr >>= 2
            nextTileAttr &= 3
          case 4 =>
            // read low bg tile
            val adr = (if (ctrl1 & CTRL1_BACKGROUND_PTA_MASK) > 0 then 0x1000 else 0) | // 0x0000 or 0x1000 base address
                      nextNtByte << 4                                                   // tile offset
            nextTileBGLow = cartridge.read(adr | fineY(),ChipID.PPU)                    // use fineY to choose correct row
          case 6 =>
            // read hi bg tile
            val adr = (if (ctrl1 & CTRL1_BACKGROUND_PTA_MASK) > 0 then 0x1000 else 0) | // 0x0000 or 0x1000 base address
              nextNtByte << 4                                                           // tile offset
            nextTileBGHi = cartridge.read((adr + 8) | fineY(),ChipID.PPU)               // use fineY to choose correct row
          case 7 =>
            // increment X
            if cycle != 256 then incH() else incV()
          case _ =>
        }
      }

      if displayON then
        cycle match
          //case 256 => // increment V
          //  incV()
          case 257 => // copy H
            copyH()
          case 338|340 =>
            // dummy NT reads
            nextNtByte = cartridge.read(NTAddress(),ChipID.PPU)
          case _ =>

      // copy V
      if preRenderLine && cycle >= 280 && cycle < 305 && displayON then copyV()

      // ============================= SPRITES =============================================
      /* Cycles 1-64: Secondary OAM (32-byte buffer for current sprites on scanline) is initialized to $FF - attempting to read $2004 will return $FF.
         Internally, the clear operation is implemented by reading from the OAM and writing into the secondary OAM as usual, only a signal is active that makes the read always return $FF
       */
      if !preRenderLine && cycle > 0 && cycle < 65 then
        //oamAddress = 0
        if ((cycle - 1) & 1) == 0 then secondOAM((cycle - 1) >> 1) = 0xFF

      //if !preRenderLine && cycle == 257 then evaluateSprite()
      if !preRenderLine then evalSprite2()

      if /*!preRenderLine &&*/ cycle > 256 && cycle < 321 && displayON then
        // OAMADDR is set to 0 during each of ticks 257-320 (the sprite tile loading interval) of the pre-render and visible scanlines.
        oamAddress = 0
        val sprite = (cycle - 257) >> 3
        (cycle - 1) & 7 match {
          case 1 =>
            cartridge.read(NTAddress(),ChipID.PPU) // garbage NT byte
          case 3 =>
            cartridge.read(NTAddress(),ChipID.PPU) // garbage NT byte
            spriteAttrLatch(sprite) = secondOAM((sprite << 2) | SPRITE_ATTR)
          case 4 =>
            spriteXLatch(sprite) = secondOAM((sprite << 2) | SPRITE_X)
          case 5 =>
            // low sprite tile byte
            fetchSprite(sprite,true)
          case 7 =>
            fetchSprite(sprite,false)
          case _ =>
        }

      // draw cycle
      if !preRenderLine && cycle > 0 && cycle < 257 then drawCycle()
    }

    // During VBlank and when rendering is disabled, the value on the PPU address bus is the current value of the v register.
    if !displayON && (rasterLine > 239) then cartridge.ppuAddressOnBus(v & 0x3FFF)

    // set vblank & NMI handling
    if rasterLine == FIRST_VBLANK_LINE && cycle == 0 then vblank = true

    if rasterLine == FIRST_VBLANK_LINE + 1 && cycle == 1 then
      vblankFlag = true
      triggerNMI()

    // increment cycle and check limits
    cycle += 1
    if cycle == 341 then
      cycle = 0
      rasterLine += 1
      if rasterLine == LAST_VBLANK_LINE + 1 then
        rasterLine = PRE_RENDER_LINE
      else
      if rasterLine == 0 then drawFrame()
  }

  inline private def fetchSprite(sprite:Int,lowByte:Boolean) : Unit = {
    val y = sprite_r(sprite,SPRITE_Y)

    if lowByte then
      if (ctrl1 & CTRL1_SPRITE_SIZE_MASK) == 0 then // 8x8 sprite size
        val spriteRow = (rasterLine - y) & 0x7
        spriteAddressLow = if (ctrl1 & CTRL1_SPRITE_PTA_MASK) > 0 then 0x1000 else 0
        spriteAddressLow |= sprite_r(sprite,SPRITE_TILE) << 4
        if (sprite_r(sprite,SPRITE_ATTR) & SPRITE_V_FLIP_MASK) == 0 then
          spriteAddressLow |= spriteRow // sprite not flipped vertically
        else
          spriteAddressLow |= 7 - spriteRow // sprite flipped vertically
      else // 8x16 sprite size
        val spriteRow = rasterLine - y
        val tile = sprite_r(sprite,SPRITE_TILE)
        spriteAddressLow = (tile & 0x1) << 12
        if (sprite_r(sprite,SPRITE_ATTR) & SPRITE_V_FLIP_MASK) == 0 then // sprite not flipped vertically
          if spriteRow < 8 then // top half
            spriteAddressLow |= (tile & 0xFE) << 4 | spriteRow
          else // bottom half
            spriteAddressLow |= ((tile & 0xFE) + 1) << 4 | spriteRow & 0x7
        else // sprite flipped vertically
          if spriteRow < 8 then // bottom half
            spriteAddressLow |= ((tile & 0xFE) + 1) << 4 | (7 - spriteRow)
          else // top half
            spriteAddressLow |= (tile & 0xFE) << 4 | (7 - (spriteRow & 0x7))

      shifterSpriteLow(sprite) = cartridge.read(spriteAddressLow,ChipID.PPU)
      //if y == 0xFF then shifterSpriteLow(sprite) = 0 // discard if sprite is not visible
    else
      shifterSpriteHi(sprite) = cartridge.read(spriteAddressLow + 8,ChipID.PPU)
      //if y == 0xFF then shifterSpriteHi(sprite) = 0 // discard if sprite is not visible
  }

  inline private def evaluateSprite() : Unit = {
    var oamPtr = oamAddress
    var spriteCount = 0
    sprite0Online = false
    val spriteHeight = if (ctrl1 & CTRL1_SPRITE_SIZE_MASK) == 0 then 8 else 16

    while oamPtr < 256 && spriteCount < 9 do
      val offset = rasterLine - OAM(oamPtr + SPRITE_Y)
      if offset >= 0 && offset < spriteHeight then
        if spriteCount < 8 then
          if oamPtr == oamAddress then sprite0Online = true // is it sprite 0

          // copy into secondary OAM
          System.arraycopy(OAM,oamPtr,secondOAM,spriteCount << 2,4)
          spriteAttrLatch(spriteCount) = OAM(oamPtr + SPRITE_ATTR)
          spriteXLatch(spriteCount) = OAM(oamPtr + SPRITE_X)
          spriteCount += 1

      oamPtr += 4

    spriteOverflowFlag = spriteCount > 8
  }


  inline private def evalSprite2(): Unit =
    if cycle >= 65 && cycle < 257 then
      if cycle == 65 then
        oamPtr = oamAddress
        secOamPtr = 0
        oamState = OAMState.FIND
        evalSprite0OnLine = false
      if (cycle & 1) == 1 then oamValue = OAM(oamPtr) // odd cycles
      else
        oamState match
          case OAMState.FIND => // find OAM in range
            secondOAM(secOamPtr) = oamValue
            val spriteHeight = if (ctrl1 & CTRL1_SPRITE_SIZE_MASK) == 0 then 8 else 16
            val offset = rasterLine - oamValue
            if offset >= 0 && offset < spriteHeight then // sprite in range
              if oamPtr == oamAddress then evalSprite0OnLine = true // is it sprite 0 ?
              oamPtr = (oamPtr + 1) & 0xFF
              secOamPtr = (secOamPtr + 1) & 0x1F
              oamState = OAMState.COPY // copy remaining bytes
            else // not in range
              oamPtr = (oamPtr + 4) & 0xFF
              if oamPtr == oamAddress then oamState = OAMState._64_EVALUATED
          case OAMState.COPY =>
            secondOAM(secOamPtr) = oamValue
            oamPtr = (oamPtr + 1) & 0xFF
            secOamPtr = (secOamPtr + 1) & 0x1F
            if (secOamPtr & 3) == 0 then
              oamState = if oamPtr == oamAddress then OAMState._64_EVALUATED // 64 sprites evaluated
              else if secOamPtr == 0 then OAMState.FIND_OVERFLOW else OAMState.FIND // 8 sprites found
          case OAMState.FIND_OVERFLOW =>
            val spriteHeight = if (ctrl1 & CTRL1_SPRITE_SIZE_MASK) == 0 then 8 else 16
            val offset = rasterLine - oamValue
            if offset >= 0 && offset < spriteHeight then // sprite in range
              spriteOverflowFlag = isDisplayOn
              oamPtr = (oamPtr + 1) & 0xFF
              secOamPtr = 0
              oamState = OAMState.READ_OVERFLOW
            else
              oamPtr = (oamPtr + 4) & 0xFF // inc n
              if oamPtr == oamAddress then oamState = OAMState._64_EVALUATED
              oamPtr = (oamPtr + 1) & 0xFF // inc m => overflow bug
          case OAMState.READ_OVERFLOW =>
            oamPtr = (oamPtr + 1) & 0xFF
            secOamPtr += 1
            if secOamPtr == 4 then
              secOamPtr = 0
              oamState = if oamPtr == oamAddress then OAMState._64_EVALUATED // 64 sprites evaluated
              else OAMState.FIND_OVERFLOW
            else
              if oamPtr == oamAddress then oamState = OAMState._64_EVALUATED // 64 sprites evaluated
          case OAMState._64_EVALUATED =>
            oamPtr = (oamPtr + 4) & 0xFF
    else if cycle == 257 then sprite0Online = evalSprite0OnLine

  inline private def getColorFrom(paletteIndex:Int,pixelIndex:Int,forceV:Boolean = false) : Int =
    /* palette RAM is accessed internally during playfield rendering (i.e., the
       palette address/data is never put on the PPU bus during this time)

       If the current VRAM address points in the range $3F00-$3FFF during forced blanking,
       the color indicated by this palette location will be shown on screen instead of the backdrop color
    */
    var colorIndex = if forceV then cartridge.read(v,ChipID.PPU_RO)
    else cartridge.read(0x3F00 | (paletteIndex << 2) | pixelIndex,ChipID.PPU_RO)
    val palette = paletteRGB((ctrl2 & CTRL2_COLOR_EMPH_MASK) >> 5)
    if (ctrl2 & CTRL2_MONOCHROME_MODE_MASK) > 0 then colorIndex &= 0x30
    palette(colorIndex & 0x3F)

  inline private def drawCycle() : Unit = {
    // BACKGROUND ======================================================
    var bgPixel = 0
    var bgPalette = 0
    val forceBgPalette = !isDisplayOn && (v & 0xFF00) == 0x3F00

    if (ctrl2 & CTRL2_BACKGROUND_ON_MASK) > 0 then // background on ?
      if (ctrl2 & CTRL2_BACKGROUND_CLIP_MASK) > 0 || cycle > 8 then
        val bitMux = 0x8000 >> finex
        val p0 = if (shifterBGLow & bitMux) > 0 then 1 else 0
        val p1 = if (shifterBGHi & bitMux) > 0 then 1 else 0

        bgPixel = (p1 << 1) | p0

        val pal0 = if (shifterAttrLow & bitMux) > 0 then 1 else 0
        val pal1 = if (shifterAttrHi & bitMux) > 0 then 1 else 0

        bgPalette = (pal1 << 1) | pal0
    // SPRITE ==========================================================
    var spritePixel = 0
    var spritePalette = 0
    var spriteHasPriorityOverBG = false
    var spriteZeroRendered = false

    if (ctrl2 & CTRL2_SPRITE_ON_MASK) > 0 then // sprite on ?
      if (ctrl2 & CTRL2_SPRITE_CLIP_MASK) > 0 || cycle > 8 then
        var s = 0
        while s < 8 do
          if spriteXLatch(s) == 0 && (cycle - 1) < 255 then // TODO: check if x < 255 is correct (see Nantettatte!! Baseball)
            val spriteAttr = spriteAttrLatch(s)
            val mask = if (spriteAttr & SPRITE_H_FLIP_MASK) > 0 then 0x01 else 0x80
            val spritePixelLo = if (shifterSpriteLow(s) & mask) > 0 then 1 else 0
            val spritePixelHi = if (shifterSpriteHi(s) & mask) > 0 then 1 else 0
            spritePixel = spritePixelHi << 1 | spritePixelLo
            spritePalette = 4 + (spriteAttr & 0x03)
            spriteHasPriorityOverBG = (spriteAttr & SPRITE_PRIORITY_MASK) == 0

            if spritePixel > 0 then // on first non-transparent pixel exit!
              if s == 0 then spriteZeroRendered = true
              s = 8 // exit earlier from loop

          s += 1
      end if
    end if

    // mix pixels
    var pixel = 0
    var palette = 0

    if bgPixel == 0 && spritePixel > 0 then
      pixel = spritePixel
      palette = spritePalette
    else
    if bgPixel > 0 && spritePixel == 0 then
      pixel = bgPixel
      palette = bgPalette
    else
    if bgPixel > 0 && spritePixel > 0 then
      if spriteHasPriorityOverBG then
        pixel = spritePixel
        palette = spritePalette
      else
        pixel = bgPixel
        palette = bgPalette

      // check sprite zero hit
      if sprite0Online && spriteZeroRendered && (ctrl2 & CTRL2_BACKGROUND_ON_MASK) > 0 && (ctrl2 & CTRL2_SPRITE_ON_MASK) > 0 && rasterLine > 0 then
        if (ctrl2 & (CTRL2_SPRITE_CLIP_MASK | CTRL2_BACKGROUND_CLIP_MASK)) != 0 then // no clipping
          sprite0HitFlag = true
        else
        if cycle > 8 then
          sprite0HitFlag = true

    val x = cycle - 1
    val point = rasterLine * DISPLAY_WIDTH + x
    val color = if x < overscan.left || x > overscan.right || rasterLine < overscan.top || rasterLine > overscan.bottom then 0xFF000000 // black
    else getColorFrom(palette,pixel,forceBgPalette)
    
    if optical != null then optical.checkColor(x,rasterLine,color)
    if pixels(point) != color then
      pixels(point) = scanLineEffect(color)
      checkCoords(cycle - 1)
  }

  inline private def drawFrame() : Unit =
    oddFrame = !oddFrame
    //javax.swing.SwingUtilities.invokeLater( () => display.showFrame(0,0,DISPLAY_WIDTH,DISPLAY_HEIGHT))
    //display.showFrame(0,0,DISPLAY_WIDTH,DISPLAY_HEIGHT)
    if frameListener != null then frameListener()

    if pixelMod then
      display.showFrame(minModX,minModY,maxModX + 1,maxModY + 1)
    else
      display.showFrame(-1,0,0,0)

    minModY = -1
    minModX = Integer.MAX_VALUE
    maxModX = 0
    pixelMod = false

  inline private def checkCoords(x:Int) : Unit =
    if (x < minModX) minModX = x
    else if (x > maxModX) maxModX = x
    if (minModY == -1) minModY = rasterLine
    maxModY = rasterLine
    pixelMod = true

  inline private def scanLineEffect(color:Int) : Int = {
    if (!applyScanLineEffect || (rasterLine & 1) == 0) color
    else {
      val r = SCANLINE_COLORS((color >> 16) & 0xFF)
      val g = SCANLINE_COLORS((color >> 8) & 0xFF)
      val b = SCANLINE_COLORS(color & 0xFF)
      0xFF << 24 | r << 16 | g << 8 | b
    }
  }

  // ================== State load & save ====================================

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(palette)
    out.writeObject(pt0)
    out.writeObject(pt1)
    out.writeBoolean(vblankFlag)
    out.writeBoolean(vblank)
    out.writeInt(shifterBGLow)
    out.writeInt(shifterBGHi)
    out.writeInt(shifterAttrLow)
    out.writeInt(shifterAttrHi)
    out.writeInt(nextTileBGLow)
    out.writeInt(nextTileBGHi)
    out.writeInt(nextTileAttr)
    out.writeInt(nextNtByte)
    out.writeInt(v)
    out.writeInt(t)
    out.writeInt(finex)
    out.writeInt(w)
    out.writeInt(ctrl1)
    out.writeInt(ctrl2)
    out.writeInt(rasterLine)
    out.writeInt(cycle)
    out.writeBoolean(oddFrame)
    out.writeObject(OAM)
    out.writeObject(secondOAM)
    out.writeInt(oamAddress)
    out.writeInt(oamPtr)
    out.writeInt(secOamPtr)
    out.writeInt(oamValue)
    out.writeBoolean(sprite0HitFlag)
    out.writeBoolean(spriteOverflowFlag)
    out.writeBoolean(sprite0Online)
    out.writeBoolean(evalSprite0OnLine)
    out.writeInt(spriteAddressLow)
    out.writeInt(lastDataOnBus)
    out.writeInt(ppuDataReadBuffer)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(palette,in)
    loadMemory(pt0,in)
    loadMemory(pt1,in)
    vblankFlag = in.readBoolean()
    vblank = in.readBoolean()
    shifterBGLow = in.readInt()
    shifterBGHi = in.readInt()
    shifterAttrLow = in.readInt()
    shifterAttrHi = in.readInt()
    nextTileBGLow = in.readInt()
    nextTileBGHi = in.readInt()
    nextTileAttr = in.readInt()
    nextNtByte = in.readInt()
    v = in.readInt()
    t = in.readInt()
    finex = in.readInt()
    w = in.readInt()
    ctrl1 = in.readInt()
    ctrl2 = in.readInt()
    rasterLine = in.readInt()
    cycle = in.readInt()
    oddFrame = in.readBoolean()
    loadMemory(OAM,in)
    loadMemory(secondOAM,in)
    oamAddress = in.readInt()
    oamPtr = in.readInt()
    secOamPtr = in.readInt()
    oamValue = in.readInt()
    sprite0HitFlag = in.readBoolean()
    spriteOverflowFlag = in.readBoolean()
    sprite0Online = in.readBoolean()
    evalSprite0OnLine = in.readBoolean()
    spriteAddressLow = in.readInt()
    lastDataOnBus = in.readInt()
    ppuDataReadBuffer = in.readInt()


  // =================== Utilities =====================================
  def getPatternTableImage(patternTableIndex:Int,palette:Int,chrBank:Option[Int] = None) : PPUTableImage =
    val pixels = Array.ofDim[Int](128 * 128)
    val baseAddress = if patternTableIndex == 0 then 0x0000 else 0x1000
    val bank = chrBank.getOrElse(-1)

    for y16 <- 0 until 16 do {
      for x16 <- 0 until 16 do {
        val offset = (y16 << 8) + (x16 << 4)
        for y <- 0 until 8 do {
          var tileLo = if bank == -1 then cartridge.read(baseAddress + offset + y,ChipID.PPU_RO) else cartridge.readCHR(bank,baseAddress + offset + y)
          var tileHi = if bank == -1 then cartridge.read(baseAddress + offset + y + 8,ChipID.PPU_RO) else cartridge.readCHR(bank,baseAddress + offset + y + 8)
          for x <- 0 until 8 do {
            val pixel0 = if (tileLo & 0x80) > 0 then 1 else 0
            val pixel1 = if (tileHi & 0x80) > 0 then 1 else 0
            tileLo <<= 1
            tileHi <<= 1
            val pixel = pixel1 << 1 | pixel0
            val pos = (((y16 << 3) + y) << 7) + (x16 << 3) + x
            pixels(pos) = getColorFrom(palette,pixel)
          }
        }
      }
    }

    PPUTableImage(pixels,128,128)

  def getNameTableImage(nt:Int,showScroll:Boolean) : PPUTableImage =
    val frameScrollX = scrollX
    val frameScrollY = scrollY
    val ntAddress = nt match
      case 0 => 0x2000
      case 1 => 0x2400
      case 2 => 0x2800
      case 3 => 0x2C00

    val pixels = Array.ofDim[Int](256 * 240)
    val tileAddress = if (ctrl1 & CTRL1_BACKGROUND_PTA_MASK) > 0 then 0x1000 else 0

    for y <- 0 until 30 do
      for x <- 0 until 32 do
        val v = x | y << 5 | nt << 10
        val tile = cartridge.read((v & 0x0FFF) | ntAddress,ChipID.PPU_RO)
        var attr = cartridge.read(0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07),ChipID.PPU_RO)
        if (y & 2) > 0 then attr >>= 4
        if (x & 2) > 0 then attr >>= 2
        attr &= 3

        for tileY <- 0 until 8 do
          val adr = tileAddress | tile << 4
          var bgLow = cartridge.read(adr + tileY,ChipID.PPU_RO)
          var bgHi = cartridge.read(adr + tileY + 8,ChipID.PPU_RO)
          for tileX <- 0 until 8 do
            val p0 = if (bgLow & 0x80) > 0 then 1 else 0
            val p1 = if (bgHi & 0x80) > 0 then 1 else 0

            val bgPixel = (p1 << 1) | p0

            val pal = attr << 2 | bgPixel
            val Y = ((y << 3) + tileY) << 8
            val X = (x << 3) + tileX
            if showScroll && (X == frameScrollX || Y == frameScrollY) then pixels(Y + X) = 0xFFFFFFFF
            else
            pixels(Y + X) = if pal == 4 || pal == 8 || pal == 0xC then
              getColorFrom(0,bgPixel)
            else getColorFrom(attr,bgPixel)

            bgLow <<= 1
            bgHi <<= 1

    PPUTableImage(pixels,256,240)
}
