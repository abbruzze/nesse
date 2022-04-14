package ucesoft.nes.mappers.fds

import ucesoft.nes.Cartridge

import java.io.{BufferedInputStream, File, FileInputStream}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object FDS:
  private inline val SIDE_SIZE = 65500
  private var biosROM : Array[Int] = _
  private var diskAccessOnScrollLockEnabled = false

  def FDS_INES() = Cartridge.iNES(0L,
    "",
    0,
    Array.ofDim[Int](0,0),
    Array(Array.ofDim[Int](8192)),
    true,
    None,
    None,
    20,
    Cartridge.Mirroring.HORIZONTAL,
    Array.ofDim[Int](0x10),
    Cartridge.TV.NTSC,
    None)

  enum DiskFileType:
    case PRAM,CRAM,VRAM

  case class DiskInfoHeaderBlock(fileNumber:Int,
                                 fileID:Int,
                                 fileName:String,
                                 fileAddress:Int,
                                 fileSize:Int,
                                 fileType:DiskFileType)

  case class DiskInfo(manufacturerCode:Int,
                      gameName:String,
                      gameVersion:Int,
                      sideNumber:Int,
                      diskNumber:Int,
                      actualDiskSide:Int,
                      bootFileID:Int,
                      blocks:List[DiskInfoHeaderBlock])

  case class FDSFile(file:String,diskInfo:List[DiskInfo],disk:Array[Array[Int]])

  def isBIOSConfigured(): Boolean = biosROM != null

  def getBiosROM(): Array[Int] = biosROM
  
  def enableScrollLockAsDiskAccess(enabled:Boolean): Unit =
    diskAccessOnScrollLockEnabled = enabled
    
  def isScrollLockEnabledAsDiskAccess : Boolean = diskAccessOnScrollLockEnabled

  def setBiosROMFile(file:String) : Unit =
    val f = new File(file)
    if f.length() != 8192 then
      throw new IllegalArgumentException(s"Bad BIOS ROM file: length must be 8192 bytes, found ${f.length()}")
    biosROM = java.nio.file.Files.readAllBytes(f.toPath).map(_.toInt & 0xFF)

  final val empty : FDSFile = {
    val disk = Array.ofDim[Int](2,65500)

    for side <- 0 to 1 do
      val d = disk(side)
      d(0) = 0x01
      val id = "*NINTENDO-HVC*"
      for i <- 0 until id.length do
        d(1 + i) = id.charAt(i)
      d(0x10) = 0x20
      d(0x11) = 0x20
      d(0x12) = 0x20
      d(0x13) = 0x20
      d(0x15) = side
      for i <- 0 until 5 do
        d(0x1A + i) = 0xFF
      d(0x23) = 0x61
      d(0x26) = 0x02
      d(0x30) = 0x80
      d(0x33) = 0x07

      d(0x38) = 0x02
    val info = DiskInfo(0,"",0,0,0,0,0,Nil)
    FDSFile("Empty",List(info,info),disk)
  }

  def loadFDSFile(file:String): FDSFile =
    val f = new File(file)
    if f.length() < 16 || f.length() < SIDE_SIZE then
      throw new IllegalArgumentException("Bad FDS file size")

    val in = new BufferedInputStream(new FileInputStream(file))
    try
      in.mark(16)
      // check if header is present
      val header = Array.ofDim[Byte](16)
      in.read(header)
      var sides = 0
      if header(0) == 0x46 && header(1) == 0x44 && header(2) == 0x53 && header(3) == 0x1A then // "FDS\n"
        sides = header(4)
      else
        // header is missing
        in.reset()
        sides = f.length().toInt / SIDE_SIZE
        if sides * SIDE_SIZE != f.length() then
          throw new IllegalArgumentException(s"Bad FDS file size: must be a multiple of $SIDE_SIZE")

      val disk = Array.ofDim[Int](sides,0xFFDC) // 65500
      var expectedBlockCode = 1
      var sideRead = 0
      var fileAmount = 0
      val diskInfoList = new ListBuffer[DiskInfo]
      val diskInfoHeaderList = new ListBuffer[DiskInfoHeaderBlock]
      var diskInfo : DiskInfo = null
      var sidePtr = 0
      var side = 0

      while side < sides do
        in.mark(1)
        val blockCode = in.read()
        in.reset()
        if  blockCode != expectedBlockCode then
          throw new IllegalArgumentException(s"Bad FDS file: expected block code $expectedBlockCode, found $blockCode")
        blockCode match
          case 1 =>
            val block = Array.ofDim[Byte](0x38)
            in.read(block)
            val DISK_VERIFICATION = "*NINTENDO-HVC*"
            for i <- 0 until DISK_VERIFICATION.length do
              if DISK_VERIFICATION.charAt(i).toByte != block(i + 1) then
                throw new IllegalArgumentException("Bad FDS file: disk verification string not found")
            val diskName = new StringBuilder
            for i <- 0x10 to 0x12 do
              val c = block(i)
              diskName.append(if c <= 0 then ' ' else c.toChar)
            diskInfo = DiskInfo(block(0xF).toInt & 0xFF,diskName.toString(),block(0x14),block(0x15),block(0x16),block(0x35),block(0x19).toInt & 0xFF,Nil)
            System.arraycopy(block.map(_.toInt & 0xFF),0,disk(side),sidePtr,0x38)
            sidePtr += 0x38
            expectedBlockCode = 2
          case 2 =>
            in.skip(1)
            fileAmount = in.read()
            disk(side)(sidePtr) = 2
            disk(side)(sidePtr + 1) = fileAmount
            sidePtr += 2
            expectedBlockCode = 3
          case 3 =>
            val block = Array.ofDim[Byte](0x10)
            in.read(block)
            val sb = new StringBuilder
            for i <- 0 to 7 do
              val c = block(3 + i).toInt & 0xFF
              sb.append(if c == 0 || c == 0xFF then ' ' else c.toChar)
            val address = (block(0x0B).toInt & 0xFF) | (block(0x0C).toInt & 0xFF) << 8
            val size = (block(0x0D).toInt & 0xFF) | (block(0x0E).toInt & 0xFF) << 8
            diskInfoHeaderList += DiskInfoHeaderBlock(block(1).toInt & 0xFF,block(2).toInt & 0xFF,sb.toString(),address,size,DiskFileType.fromOrdinal(block(0x0F)))
            System.arraycopy(block.map(_.toInt & 0xFF),0,disk(side),sidePtr,0x10)
            sidePtr += 0x10
            expectedBlockCode = 4
          case 4 =>
            val block = Array.ofDim[Byte](diskInfoHeaderList.last.fileSize + 1)
            in.read(block)
            System.arraycopy(block.map(_.toInt & 0xFF),0,disk(side),sidePtr,block.length)
            sidePtr += block.length
            fileAmount -= 1
            if fileAmount == 0 then
              diskInfo = diskInfo.copy(blocks = diskInfoHeaderList.toList)
              diskInfoHeaderList.clear()
              diskInfoList += diskInfo
              expectedBlockCode = 1
              // skip spaces
              in.read(Array.ofDim[Byte](SIDE_SIZE - sidePtr))

              side += 1
              sidePtr = 0
            else
              expectedBlockCode = 3
      FDSFile(file,diskInfoList.toList,disk)
    finally
      in.close()

  def main(args:Array[String]): Unit =
    for info <- loadFDSFile(args(0)).diskInfo do
      println(s"${info.gameName}/${info.gameVersion} #${info.diskNumber}/${info.sideNumber} bootID=${info.bootFileID}")
      for block <- info.blocks do
        println(s"\t${block.fileName}/${block.fileNumber}/${block.fileID} [${block.fileAddress.toHexString.toUpperCase()}/${block.fileSize}] ${block.fileType}")
