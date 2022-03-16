package ucesoft.nes

import ucesoft.nes.PPU.PPUType

import java.io.{File, FileInputStream, InputStream}
import scala.collection.mutable

class Palette private (val palette:Array[Array[Int]])

object Palette:
  inline private def dark(v:Int) : Int = (v * 0.7).toInt

  /**
   * PPUMASK:
   * 7  bit  0
   * ---- ----
   * BGRs bMmG
   * |||| ||||
   * |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
   * |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
   * |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
   * |||| +---- 1: Show background
   * |||+------ 1: Show sprites
   * ||+------- Emphasize red (green on PAL/Dendy)
   * |+-------- Emphasize green (red on PAL/Dendy)
   * +--------- Emphasize blue
   *
   * Each bit emphasizes 1 color while darkening the other two.
   * Setting all three emphasis bits will darken colors $00-$0D, $10-$1D, $20-$2D, and $30-$3D
   */
  private final val _2C02_PALETTE : Palette = makePaletteFromRGB(getClass().getResourceAsStream("/resources/palette/2C02_palette.txt"))
  private final val _2C03_2C05_PALETTE : Palette = makePaletteFromRGBS(getClass().getResourceAsStream("/resources/palette/2C03_2C05_rgbs_palette.txt"))
  private final val _2C03_B_PALETTE : Palette = makePaletteFromRGBS(getClass().getResourceAsStream("/resources/palette/2C03_B_rgbs_palette.txt"))
  private final val _2C04_1_PALETTE : Palette = makePaletteFromRGBS(getClass().getResourceAsStream("/resources/palette/2C04_1_rgbs_palette.txt"))
  private final val _2C04_2_PALETTE : Palette = makePaletteFromRGBS(getClass().getResourceAsStream("/resources/palette/2C04_2_rgbs_palette.txt"))
  private final val _2C04_3_PALETTE : Palette = makePaletteFromRGBS(getClass().getResourceAsStream("/resources/palette/2C04_3_rgbs_palette.txt"))
  private final val _2C04_4_PALETTE : Palette = makePaletteFromRGBS(getClass().getResourceAsStream("/resources/palette/2C04_4_rgbs_palette.txt"))
  
  private val paletteMap = new mutable.HashMap[PPUType,Palette]
  
  paletteMap += PPUType._2C02 -> _2C02_PALETTE
  paletteMap += PPUType._2C03 -> _2C03_2C05_PALETTE
  paletteMap += PPUType._2C05 -> _2C03_2C05_PALETTE
  paletteMap += PPUType._2C03B -> _2C03_B_PALETTE
  paletteMap += PPUType._2C04_0001 -> _2C04_1_PALETTE
  paletteMap += PPUType._2C04_0002 -> _2C04_2_PALETTE
  paletteMap += PPUType._2C04_0003 -> _2C04_3_PALETTE
  paletteMap += PPUType._2C04_0004 -> _2C04_4_PALETTE
  
  def getPaletteFor(ppuType:PPUType): Palette = paletteMap(ppuType)
  def setPaletteFor(ppuType:PPUType,palette:Palette):Unit = paletteMap += ppuType -> palette

  private def fromRaw(raw:Array[(Int,Int,Int)]): Array[Array[Int]] =
    (for i <- 0 to 7 yield
      i match
        case 0 => // BGR = 000, standard colors
          raw map { rgb => 0xFF << 24 | rgb._1 << 16 | rgb._2 << 8 | rgb._3 }
        case 1 => // BGR = 001, emph on RED
          raw map { rgb => 0xFF << 24 | rgb._1 << 16 | dark(rgb._2) << 8 | dark(rgb._3) }
        case 2 => // BGR = 010, emph on GREEN
          raw map { rgb => 0xFF << 24 | dark(rgb._1) << 16 | rgb._2 << 8 | dark(rgb._3) }
        case 3 => // BGR = 011, emph on GREEN and RED
          raw map { rgb => 0xFF << 24 | rgb._1 << 16 | rgb._2 << 8 | dark(rgb._3) }
        case 4 => // BGR = 100, emph on BLUE
          raw map { rgb => 0xFF << 24 | dark(rgb._1) << 16 | dark(rgb._2) << 8 | rgb._3 }
        case 5 => // BGR = 101, emph on BLUE and RED
          raw map { rgb => 0xFF << 24 | rgb._1 << 16 | dark(rgb._2) << 8 | rgb._3 }
        case 6 => // BGR = 110, emph on BLUE and GREEN
          raw map { rgb => 0xFF << 24 | dark(rgb._1) << 16 | rgb._2 << 8 | rgb._3 }
        case 7 => // BGR = 111
          raw map { rgb => 0xFF << 24 | dark(rgb._1) << 16 | dark(rgb._2) << 8 | dark(rgb._3) }
      ).toArray

  private def makeRaw(rgb:Array[Int]): Array[(Int,Int,Int)] =
    val rgbGroup = rgb.sliding(3,3).map { _ match
      case Array(r,g,b) => (r,g,b)
      case _ => (0,0,0)
    }
    rgbGroup.toArray

  private def makePaletteFromBinary(in:InputStream):Palette =
    val buffer = in.readAllBytes().map(_.toInt & 0xFF)
    if buffer.length != 192 then
      throw new IllegalArgumentException("Invalid binary palette file")

    new Palette(fromRaw(makeRaw(buffer)))

  private def makePaletteFromRGB(in:InputStream): Palette =
    val lines = io.Source.fromInputStream(in).getLines().filterNot(_.startsWith("#"))
    val raw = for line <- lines yield
      line.split(",") match
        case Array(r,g,b) =>
          (Integer.parseInt(r,16),Integer.parseInt(g,16),Integer.parseInt(b,16))
        case _ =>
          throw new IllegalArgumentException("Invalid RGB palette file")

    val pal = raw.toArray
    if pal.length != 64 then
      throw new IllegalArgumentException("Invalid binary palette file")
    new Palette(fromRaw(pal))

  private def makePaletteFromRGBS(in:InputStream): Palette =
    val value = io.Source.fromInputStream(in).getLines().filterNot(_.startsWith("#")).mkString(",")
    val raw = value.split(",") map { v =>
      if v.length != 3 then
        throw new IllegalArgumentException("Invalid RGBS palette file")
      val rgb = v map { c =>
        val digit = c - '0'
        if digit > 7 then
          throw new IllegalArgumentException("Invalid RGBS palette file")
        (digit * 0xFF) / 7
      }
      (rgb(0),rgb(1),rgb(2))
    }
    if raw.length != 64 then
      throw new IllegalArgumentException("Invalid binary palette file")
    new Palette(fromRaw(raw))

  def makePaletteFromBinaryFile(file:String):Palette =
    val pal = new File(file)
    if !pal.exists() || pal.length() != 192 then
      throw new IllegalArgumentException("Invalid binary palette file")

    val in = new FileInputStream(pal)
    try
      makePaletteFromBinary(in)
    finally
      in.close()
