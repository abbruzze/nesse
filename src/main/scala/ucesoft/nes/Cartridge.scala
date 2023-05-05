package ucesoft.nes

import ucesoft.nes.cpu.{CPU6502, Memory}
import ucesoft.nes.mappers.fds.FDS
import ucesoft.nes.util.CartDB
import ucesoft.nes.util.CartDB.Game

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, RandomAccessFile}
import java.util.Properties
import java.util.zip.{CRC32, GZIPInputStream, GZIPOutputStream}

object Cartridge:

  case class Overscan(top:Int,bottom:Int,left:Int,right:Int)

  enum Mirroring:
    case HORIZONTAL, VERTICAL, FOUR_SCREEN, ALL_MIRROR0, ALL_MIRROR1, DYNAMIC

  enum TV(val ppuClockFrequency:Int,val cpuDivider:Float,val totalScanLines:Int,val overScan:Overscan):
    case NTSC extends TV(5_369_318,3,262,Overscan(8,240 - 9,0,256))
    case PAL extends TV(5_320_342,3.2,312,Overscan(1,240,2,253))

  private final val PAL_FILE_KEYWORD = Array("(E)", "(EUROPE)", "(PAL)", "(F)", "(G)", "(I)")

  case class iNES(crc:Long,
                  file:String,
                  version:Int,
                  prgROM:Array[Array[Int]],
                  chrROM:Array[Array[Int]],
                  chrIsRAM:Boolean,
                  trainer:Option[Array[Int]],
                  prgRAM:Option[Array[Int]],
                  mapperID:Int,
                  mirroring:Mirroring,
                  header:Array[Int],
                  tv:TV,
                  game:Option[Game]):

      override def toString : String =
        s"iNES[${game.map(_.name).getOrElse(crc.toHexString.toUpperCase())}](tv:$tv ver:$version mapperID:$mapperID mirroring:$mirroring prgBanks:${prgROM.length} chrBanks:${chrROM.length}[as ram $chrIsRAM] trainer:${if trainer.isDefined then "yes" else "no"} prgRAM:${if prgRAM.isDefined then "yes" else "no"} file:$file${if game.isDefined then s"\n$game" else ""}"

  def loadINES(file:String,overwriteTV:Option[TV] = None) : iNES =
    val in = new RandomAccessFile(file,"r")
    try
      val header = Array.ofDim[Byte](0x10)
      in.read(header)

      if header(0) != 0x4E || header(1) != 0x45 || header(2) != 0x53 || header(3) != 0x1A then throw new IllegalArgumentException("Bad ines signature format")
      // check version
      val version = if (header(7) & 0x0C) == 0x08 then 2 else 1
      if version == 2 then throw new IllegalArgumentException("INES format 2.0 not supported yet")

      loadINESv1(file,in,header map { _.toInt & 0xFF },overwriteTV)
    finally in.close()

  /**
      HEADER(4) = Size of PRG ROM in 16 KB units
      HEADER(5) = Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)

      HEADER(6)
      76543210
      ||||||||
      |||||||+- Mirroring: 0: horizontal (vertical arrangement) (CIRAM A10 = PPU A11)
      |||||||              1: vertical (horizontal arrangement) (CIRAM A10 = PPU A10)
      ||||||+-- 1: Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
      |||||+--- 1: 512-byte trainer at $7000-$71FF (stored before PRG data)
      ||||+---- 1: Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
      ++++----- Lower nybble of mapper number

      HEADER(7)
      76543210
      ||||||||
      |||||||+- VS Unisystem
      ||||||+-- PlayChoice-10 (8KB of Hint Screen data stored after CHR data)
      ||||++--- If equal to 2, flags 8-15 are in NES 2.0 format
      ++++----- Upper nybble of mapper number

      HEADER(8)
      76543210
      ||||||||
      ++++++++- PRG RAM size

      HEADER(9)
      76543210
      ||||||||
      |||||||+- TV system (0: NTSC; 1: PAL)
      +++++++-- Reserved, set to zero

      HEADER(10)
      76543210
      ||  ||
      ||  ++- TV system (0: NTSC; 2: PAL; 1/3: dual compatible)
      |+----- PRG RAM ($6000-$7FFF) (0: present; 1: not present)
      +------ 0: Board has no bus conflicts; 1: Board has bus conflicts
   */
  private def loadINESv1(file:String,in:RandomAccessFile,header:Array[Int],overwriteTV:Option[TV] = None) : iNES =
    val trainer = if (header(6) & 0x04) > 0 then
      val buffer = Array.ofDim[Byte](512)
      in.read(buffer)
      Some(buffer map { _.toInt & 0xFF})
    else
      None

    val prgROM = Array.ofDim[Byte](header(4) * 16384)
    in.read(prgROM)
    val chrROMasRAM = header(5) == 0
    var chrROM = Array.ofDim[Byte](header(5) * 8192)
    if !chrROMasRAM then
      in.read(chrROM)

    var mirroring = if (header(6) & 0x08) > 0 then Mirroring.FOUR_SCREEN
                    else
                    if (header(6) & 0x01) == 0 then Mirroring.HORIZONTAL else Mirroring.VERTICAL

    val prgRAM = if (header(6) & 0x02) > 0 then Some(Array.fill[Int](0x2000)(0xFF)) else None

    var tv = if (header(9) & 1) == 0 then TV.NTSC else TV.PAL

    // TODO check if correct
    for ram <- prgRAM; tr <- trainer do
      System.arraycopy(tr,0,ram,0x1000,tr.length) // copy trainer bytes into 0x7000 - 0x71FF

    var mapperID = header(7) & 0xF0 | (header(6) >> 4)

    // CRC
    val crc = new CRC32
    crc.update(prgROM)
    if !chrROMasRAM then crc.update(chrROM)

    val game = CartDB.gameDB.get(crc.getValue)
    game match
      case None =>
        println(s"Game not found: crc = ${crc.getValue.toHexString.toUpperCase()}")
        if chrROMasRAM then
          chrROM = Array.ofDim[Byte](8192) // 8K chr ram
        // check PAL keyword
        if PAL_FILE_KEYWORD.exists(file.toUpperCase().indexOf(_) != -1) then
          tv = TV.PAL
      case Some(g) =>
        println(s"Found on DB: ${g.name}")
        if chrROMasRAM then
          val sizeK = g.vramSizeK match
            case Some(vr) => vr / 8
            case None => 1
          chrROM = Array.ofDim[Byte](sizeK * 8192)
        mapperID = g.mapper
        tv = if g.tv.toUpperCase().indexOf("PAL") != -1 then TV.PAL else TV.NTSC
        g.padH match
          case Some(pad) =>
            mirroring = if pad == 0 then Mirroring.HORIZONTAL else Mirroring.VERTICAL
            println(s"New mirroring to $mirroring")
          case None =>
            g.vsGame match
              case Some(vs) =>
                mirroring = vs.mirroring
              case None =>
    if overwriteTV.isDefined then
      tv = overwriteTV.get
    iNES(crc.getValue,file,1,bufferToBanks(prgROM,16384),bufferToBanks(chrROM,8192),chrROMasRAM,trainer,prgRAM,mapperID,mirroring,header,tv,game)

  private def bufferToBanks(buffer:Array[Byte],size:Int) : Array[Array[Int]] =
    val bankSize = buffer.length / size
    val banks = Array.ofDim[Int](bankSize,size)
    for b <- 0 until bankSize do
      for i <- 0 until size do
        banks(b)(i) = buffer(b * size + i).toInt & 0xFF
    banks

abstract class Cartridge(protected val ppu : PPU,val ines:Cartridge.iNES,protected val irqHandler: Boolean => Unit) extends NESComponent with Memory:
  import Cartridge.*
  
  val cartName : String
  val isFDS = false
  val hasAudio = false
  val listenToAllAddresses = false

  override val name: String = "Cartridge"
  override val componentID: String = s"Cartridge"
  override val componentType: NESComponentType = NESComponentType.CARTRIDGE

  protected final val CHR : Array[Array[Int]] = checkCHRRom(ines.chrROM)
  protected final val ROM : Array[Array[Int]] = checkPRGRom(ines.prgROM)
  protected final val SRAM: Array[Int] = initStaticRam()
  protected final val CHR_SIZE = CHR.length
  protected final val ROM_SIZE = ROM.length
  protected var sramEnabled = true
  protected var sramReadOnly = false
  protected val _16k = ines.prgROM.length == 1
  protected var mirroring : Mirroring = Mirroring.HORIZONTAL

  protected var pt2 : Array[Int] = _
  protected var pt3 : Array[Int] = _

  protected var chrBankIndex = 0
  protected var romBankIndex = 0
  
  protected var openbus = 0

  protected var autoSaveRAM = true

  protected var customNTHandling = false
  
  // Cart audio
  def getAudioSample(): Double = 0.0
  def getAudioRatio(): Double = 1.0
  
  // FDS
  def insertFDS(fds:FDS.FDSFile): Unit = {}
  def setDiskSide(side:Int): Unit = {}
  def ejectFDS(): Unit = {}
  def getDiskSide(): Int = 0
  def getFDS(): Option[FDS.FDSFile] = None

  def setAutoSaveRAM(on:Boolean): Unit = autoSaveRAM = on

  // CONSTRUCTOR ========================
  if ines.mirroring == Mirroring.FOUR_SCREEN then
    pt2 = Array.ofDim[Int](0x400)
    pt3 = Array.ofDim[Int](0x400)

  setMirroring(ines.mirroring)
  checkSavedRAM()
  // ====================================

  override def getProperties: Properties =
    val p = super.getProperties
    p.setProperty("Name",cartName)
    p.setProperty("Famicom Disk System",isFDS.toString)
    p.setProperty("Aux audio",hasAudio.toString)
    p.setProperty("Mirroring",mirroring.toString)
    p.setProperty("File",ines.file)
    p.setProperty("TV system",ines.tv.toString)
    p.setProperty("Game",ines.game.map(_.name).getOrElse("N/A"))
    p.setProperty("Mapper #",ines.mapperID.toString)
    p.setProperty("Char ROM as RAM",ines.chrIsRAM.toString)
    p.setProperty("Char ROM size",s"${CHR.length * 8}K")
    p.setProperty("PRG ROM size",s"${ROM.length * 16}K")
    p.setProperty("RAM size",s"${ines.prgRAM.map(_.length).getOrElse(0) / 1024}K")
    p
    
  def readCHR(bank:Int,offset:Int): Int = CHR(bank % CHR.length)(offset)
  def chrBankSize: Int = CHR.length

  protected def checkPRGRom(rom:Array[Array[Int]]): Array[Array[Int]] =
    if rom.length > 1 || rom.length == 0 then
      rom
    else
      val prg = Array.ofDim[Int](2,16384)
      System.arraycopy(rom(0),0,prg(0),0,16384)
      System.arraycopy(rom(0),0,prg(1),0,16384)
      prg

  protected def checkCHRRom(rom:Array[Array[Int]]): Array[Array[Int]] = rom

  protected def getSavedRAMFileName(): File = new File(s"${ines.file}_ram")

  protected def checkSavedRAM(): Unit =
    if hasMemoryToSave() then
      val savedRAM = getSavedRAMFileName()
      if savedRAM.exists() then
        try
          val in = new ObjectInputStream(new GZIPInputStream(new FileInputStream(savedRAM)))
          loadRAM(in)
          in.close()
        catch
          case t:Throwable =>
            println(s"Can't load saved RAM from $savedRAM [$t]")
  
  protected def initStaticRam() : Array[Int] =
    ines.prgRAM match {
      case None => Array.ofDim[Int](0x2000)
      case Some(ram) => ram
    }

  override def toString : String = s"Cart($cartName) = $ines"

  def getMirroring(): Mirroring = mirroring

  protected def setMirroring(mirroring:Mirroring) : Unit =
    import Mirroring.*
    this.mirroring = mirroring
    
    mirroring match
      case HORIZONTAL =>
        ppu.nt_2000 = ppu.pt0
        ppu.nt_2400 = ppu.nt_2000
        ppu.nt_2800 = ppu.pt1
        ppu.nt_2C00 = ppu.nt_2800
      case VERTICAL =>
        ppu.nt_2000 = ppu.pt0
        ppu.nt_2400 = ppu.pt1
        ppu.nt_2800 = ppu.nt_2000
        ppu.nt_2C00 = ppu.nt_2400
      case FOUR_SCREEN =>
        ppu.nt_2000 = ppu.pt0
        ppu.nt_2400 = ppu.pt1
        ppu.nt_2800 = pt2
        ppu.nt_2C00 = pt3
      case ALL_MIRROR0 =>
        ppu.nt_2000 = ppu.pt0
        ppu.nt_2400 = ppu.pt0
        ppu.nt_2800 = ppu.pt0
        ppu.nt_2C00 = ppu.pt0
      case ALL_MIRROR1 =>
        ppu.nt_2000 = ppu.pt1
        ppu.nt_2400 = ppu.pt1
        ppu.nt_2800 = ppu.pt1
        ppu.nt_2C00 = ppu.pt1
      case _ =>

  override final def read(address: Int, chipID: ChipID): Int = chipID match
    case ChipID.CPU =>
      openbus = readCPU(address)
      openbus
    case ChipID.PPU => readPPU(address)
    case ChipID.PPU_RO => readPPU(address,true)


  override final def write(address: Int, value: Int, chipID: ChipID): Unit = chipID match
    case ChipID.CPU =>
      writeCPU(address,value)
      openbus = value
    case ChipID.PPU => writePPU(address,value)
  
  def ppuAddressOnBus(addressOnBus:Int) : Unit = {}
  
  def ppuClock(ppuClockCycle:Int,rasterLine:Int): Unit = {}
  
  def cpuClock(): Unit = {}

  protected def hasMemoryToSave(): Boolean = ines.prgRAM.isDefined

  def eject() : Unit =
    if autoSaveRAM && hasMemoryToSave() then
      try
        val out = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(getSavedRAMFileName())))
        saveRAM(out)
        out.close()
      catch
        case t:Throwable =>
          println(s"Can't save RAM on file ${getSavedRAMFileName()}")

  override def reset: Unit =
    sramEnabled = true
    chrBankIndex = 0
    romBankIndex = 0
    
  override def init: Unit = {}

  protected def readNT(address:Int,index:Int): Int = 0
  protected def writeNT(address:Int,index:Int,value:Int): Unit = {}

  protected def readPPU(_address:Int,readOnly:Boolean = false) : Int =
    var address = _address & 0x3FFF // manage mirrors area at 0x4000 - 0xFFFF
    if address >= 0x3000 && address < 0x3F00 then address -= 0x1000 // manage mirrors area at 0x3000 - 0x3F00

    if address < 0x2000 then readCHR(address) // pattern tabels
    else
    if address < 0x2400 then
      if customNTHandling then readNT(address,0) else ppu.nt_2000(address & 0x3FF) // name table area at 0x2000 - 0x2400
    else
    if address < 0x2800 then
      if customNTHandling then readNT(address,1) else ppu.nt_2400(address & 0x3FF) // name table area at 0x2400 - 0x2800
    else
    if address < 0x2C00 then
      if customNTHandling then readNT(address,2) else ppu.nt_2800(address & 0x3FF) // name table area at 0x2800 - 0x2C00
    else
    if address < 0x3000 then
      if customNTHandling then readNT(address,3) else ppu.nt_2C00(address & 0x3FF) // name table area at 0x2C00 - 0x3000
    else // palette address range
      address &= 0x1F
      ppu.palette(getPaletteIndex(address))
      

  inline private def getPaletteIndex(index:Int) : Int =
      index match {
        case 0x10 => 0x00
        case 0x14 => 0x04
        case 0x18 => 0x08
        case 0x1C => 0x0C
        case i => i
      }

  protected def writePPU(_address:Int,value:Int) : Unit =
    var address = _address & 0x3FFF // manage mirrors area at 0x4000 - 0xFFFF
    if address >= 0x3000 && address < 0x3F00 then address -= 0x1000 // manage mirrors area at 0x3000 - 0x3F00

    if address < 0x2000 then writeCHR(address,value) // pattern tabels
    else
    if address < 0x2400 then
      if customNTHandling then writeNT(address,0,value) else ppu.nt_2000(address & 0x3FF) = value // name table area at 0x2000 - 0x2400
    else
    if address < 0x2800 then
      if customNTHandling then writeNT(address,1,value) else ppu.nt_2400(address & 0x3FF) = value // name table area at 0x2400 - 0x2800
    else
    if address < 0x2C00 then
      if customNTHandling then writeNT(address,2,value) else ppu.nt_2800(address & 0x3FF) = value // name table area at 0x2800 - 0x2C00
    else
    if address < 0x3000 then
      if customNTHandling then writeNT(address,3,value) else ppu.nt_2C00(address & 0x3FF) = value // name table area at 0x2C00 - 0x3000
    else // palette address range
      address &= 0x1F
      val target = getPaletteIndex(address)
      ppu.palette(target) = value & 0x3F
      if target != address then ppu.palette(address) = value & 0x3F // for palette viewer

  protected def readCHR(address:Int) : Int =
    CHR(chrBankIndex % CHR.length)(address)

  protected def writeCHR(address:Int,value:Int) : Unit =
    if ines.chrIsRAM then
      CHR(chrBankIndex % CHR.length)(address) = value

  protected def readRAM(address:Int) : Int =
    if SRAM != null && sramEnabled then SRAM(address & 0x1FFF) else openbus

  protected def readCPU(address:Int) : Int =
    if address < 0x6000 then address >> 8 // TODO Expansion ROM
    else
    if address < 0x8000 then
      readRAM(address)
    else
      readPRG(address)

  protected def writeCPU(address:Int,value:Int) : Unit =
    if address >= 0x6000 && address < 0x8000 then
      if SRAM != null && sramEnabled && !sramReadOnly then SRAM(address - 0x6000) = value

  protected def readPRG(address: Int) : Int =
    if address < 0xC000 then ROM(0)(address & 0x3FFF)
    else
      if _16k then ROM(0)(address & 0x3FFF) else ROM(1)(address & 0x3FFF)

  override def saveState(out: ObjectOutputStream): Unit =
    if ines.chrIsRAM then
      out.writeObject(CHR)
      
    if SRAM != null then
      out.writeObject(SRAM)

    out.writeObject(pt2)
    out.writeObject(pt3)
      
    out.writeBoolean(sramEnabled)
    out.writeBoolean(sramReadOnly)
    out.writeInt(chrBankIndex)
    out.writeInt(romBankIndex)
    out.writeInt(openbus)
    out.writeObject(mirroring)
    out.writeBoolean(customNTHandling)

  override def loadState(in: ObjectInputStream): Unit =
    if ines.chrIsRAM then
      loadMemory(CHR,in)
      
    if SRAM != null then
      loadMemory(SRAM,in)

    pt2 = in.readObject().asInstanceOf[Array[Int]]
    pt3 = in.readObject().asInstanceOf[Array[Int]]

    sramEnabled = in.readBoolean()
    sramReadOnly = in.readBoolean()
    chrBankIndex = in.readInt()
    romBankIndex = in.readInt()
    openbus = in.readInt()
    mirroring = in.readObject().asInstanceOf[Mirroring]
    setMirroring(mirroring)
    customNTHandling = in.readBoolean()

  protected def saveRAM(out:ObjectOutputStream): Unit =
    if SRAM != null then
      out.writeObject(SRAM)

  protected def loadRAM(in:ObjectInputStream): Unit =
    loadMemory(SRAM,in)

  final protected def readCHR_1K(address:Int,r:Int) : Int =
    CHR((r >> 3)%CHR_SIZE)(((r & 7) << 10) | (address & 0x3FF))

  final protected def readCHR_2K(address:Int,r:Int) : Int =
    CHR((r >> 2)%CHR_SIZE)(((r & 3) << 11) | (address & 0x7FF))

  final protected def readCHR_4K(address:Int,r:Int) : Int =
    CHR((r >> 1)%CHR_SIZE)(((r & 1) << 12) | (address & 0xFFF))

  final protected def writeCHR_1K(address:Int,r:Int,value:Int) : Unit =
    CHR((r >> 3)%CHR_SIZE)(((r & 7) << 10) | (address & 0x3FF)) = value

  final protected def writeCHR_2K(address:Int,r:Int,value:Int) : Unit =
    CHR((r >> 2)%CHR_SIZE)(((r & 3) << 11) | (address & 0x7FF)) = value

  final protected def writeCHR_4K(address:Int,r:Int,value:Int) : Unit =
    CHR((r >> 1)%CHR_SIZE)(((r & 1) << 12) | (address & 0xFFF)) = value

  final protected def readPRG_8K(address:Int, r:Int) : Int =
    ROM((r >> 1) %ROM_SIZE)(((r & 1) << 13) | (address & 0x1FFF))

  final protected def readPRG_32K(address:Int, r:Int) : Int =
    if (address & 0x7FFF) < 0x4000 then ROM((r << 1) % ROM_SIZE)(address & 0x3FFF)
    else ROM(((r << 1) + 1)  % ROM_SIZE)(address & 0x3FFF)