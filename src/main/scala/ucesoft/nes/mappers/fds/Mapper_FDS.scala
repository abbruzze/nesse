package ucesoft.nes.mappers.fds

import ucesoft.nes.mappers.sound.FDSAudio
import ucesoft.nes.{Cartridge, NESComponent, PPU}

import java.awt.{Robot, Toolkit}
import java.awt.event.KeyEvent
import java.io.{ObjectInputStream, ObjectOutputStream}
import javax.swing.SwingUtilities

class Mapper_FDS(bios:Array[Int],ppu : PPU,irqHandler: Boolean => Unit) extends Cartridge(ppu,FDS.FDS_INES(),irqHandler):
  override val cartName : String = "FDS"
  override val isFDS = true
  override val hasAudio = true

  private[this] inline val BYTE_CYCLES = 150 // 1_790_000.0 / 96400.0 * 8
  private[this] inline val LED_DELAY = 1_790_000 / 1000 // 1ms
  private[this] inline val CHANGE_DISK_CYCLES = 1_790_000

  private[this] final val ram = Array.ofDim[Int](0x8000)

  private[this] var diskIORegistersEnabled = false
  // IRQ
  private[this] var irqTimerLatch = 0
  private[this] var irqTimerOccurred, irqTransferOccurred = false
  private[this] var irqTimerEnabled, irqTranferEnabled = false
  private[this] var irqTimerRepeatFlag = false
  private[this] var irqTimerCounter = 0
  // FDS
  private[this] var fds : Option[FDS.FDSFile] = None
  private[this] var targetFDS : FDS.FDSFile = _
  private[this] var changeDiskCycles = 0
  private[this] var diskSide, targetDiskSide = 0
  private[this] var diskPointer = 0
  private[this] var disk: Array[Array[Int]] = _
  private[this] var written = false
  // Disk controller
  private[this] var readMode = false
  private[this] var motorOn = false
  private[this] var scanningMedia = false
  private[this] var crcControl = false
  private[this] var isOnGAP = false
  private[this] var readyFlag = false
  private[this] var irqDiskSeek,writeSkip = 0
  // Audio
  private[this] val audio = new FDSAudio

  private[this] var ledCounter = 0
  private[this] var ledON = false
  private[this] val robot = new Robot

  override def saveState(out: ObjectOutputStream): Unit =
    super.saveState(out)
    out.writeObject(ram)
    out.writeObject(fds)
    out.writeInt(irqTimerLatch)
    out.writeBoolean(irqTimerOccurred)
    out.writeBoolean(irqTransferOccurred)
    out.writeBoolean(irqTimerEnabled)
    out.writeBoolean(irqTranferEnabled)
    out.writeBoolean(irqTimerRepeatFlag)
    out.writeInt(irqTimerCounter)
    out.writeInt(diskSide)
    out.writeInt(diskPointer)
    out.writeInt(irqDiskSeek)
    out.writeInt(writeSkip)
    out.writeBoolean(written)
    out.writeBoolean(readMode)
    out.writeBoolean(motorOn)
    out.writeBoolean(scanningMedia)
    out.writeBoolean(crcControl)
    out.writeBoolean(isOnGAP)
    out.writeBoolean(readyFlag)
    audio.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    super.loadState(in)
    NESComponent.loadMemory(ram,in)
    fds = in.readObject().asInstanceOf[Option[FDS.FDSFile]]
    if fds.isDefined then
      disk = fds.get.disk
    irqTimerLatch = in.readInt()
    irqTimerOccurred = in.readBoolean()
    irqTransferOccurred = in.readBoolean()
    irqTimerEnabled = in.readBoolean()
    irqTranferEnabled = in.readBoolean()
    irqTimerRepeatFlag = in.readBoolean()
    irqTimerCounter = in.readInt()
    diskSide = in.readInt()
    diskPointer = in.readInt()
    irqDiskSeek = in.readInt()
    writeSkip = in.readInt()
    written = in.readBoolean()
    readMode = in.readBoolean()
    motorOn = in.readBoolean()
    scanningMedia = in.readBoolean()
    crcControl = in.readBoolean()
    isOnGAP = in.readBoolean()
    readyFlag = in.readBoolean()
    audio.loadState(in)

  override def reset: Unit =
    audio.reset()
    diskIORegistersEnabled = false
    irqTimerLatch = 0
    irqTimerOccurred = false
    irqTransferOccurred = false
    irqTimerEnabled = false
    irqTranferEnabled = false
    irqTimerRepeatFlag = false
    irqTimerCounter = 0
    readMode = false
    motorOn = false
    scanningMedia = false
    crcControl = false
    isOnGAP = false
    readyFlag = false
    irqDiskSeek = 0
    writeSkip = 0
    ledCounter = 0
    ledON = false

  inline private def setLedOn(on:Boolean): Unit =
    if FDS.isScrollLockEnabledAsDiskAccess then
      if on then
        ledCounter = LED_DELAY
      if on != ledON then
        ledON = on
        SwingUtilities.invokeLater(() => {
          robot.keyPress(KeyEvent.VK_SCROLL_LOCK)
          robot.keyRelease(KeyEvent.VK_SCROLL_LOCK)
          if !on then
            robot.keyPress(KeyEvent.VK_SCROLL_LOCK)
            robot.keyRelease(KeyEvent.VK_SCROLL_LOCK)
        })

  override def getAudioSample(): Double = audio.getAudioSample()
  override def getAudioRatio(): Double = 0.60

  inline private def isDiskInserted() : Boolean = fds.isDefined

  private def checkDisk(): Unit =
    fds match
      case Some(fds) if written =>
        import java.io.*
        val out = new FileOutputStream(fds.file + ".sav")
        for side <- disk.indices do
          val buffer = fds.disk(side).map(_.toByte)
          out.write(buffer)
        out.close()
      case _ =>

  override def getFDS(): Option[FDS.FDSFile] = fds

  override def insertFDS(fds:FDS.FDSFile): Unit =
    targetFDS = fds
    targetDiskSide = 0
    changeDiskCycles = CHANGE_DISK_CYCLES
    ejectFDS()

  override def ejectFDS(): Unit =
    checkDisk()
    fds = None

  override def setDiskSide(side:Int): Unit =
    targetDiskSide = side
    targetFDS = fds.get
    fds = None
    changeDiskCycles = CHANGE_DISK_CYCLES
    
  override def getDiskSide(): Int = diskSide

  override final def cpuClock(): Unit =
    // LED
    if ledCounter > 0 then
      ledCounter -= 1
      if ledCounter == 0 then
        setLedOn(false)
    // Audio
    audio.clock()
    // timer IRQ =========================================
    if diskIORegistersEnabled && irqTimerEnabled && irqTimerCounter > 0 then
      irqTimerCounter -= 1
      if irqTimerCounter == 0 then
        irqTimerOccurred = true
        checkIRQ()
        if !irqTimerRepeatFlag then
          irqTimerEnabled = false
        else
          irqTimerCounter = irqTimerLatch
    // IRQ SEEK ==========================================
    if irqDiskSeek > 0 then
      irqDiskSeek -= 1
      if irqDiskSeek == 0 && irqTranferEnabled then
        irqTransferOccurred = true
        checkIRQ()
    // CHANGE DISK =======================================
    if changeDiskCycles > 0 then
      changeDiskCycles -= 1
      if changeDiskCycles == 0 then
        fds = Some(targetFDS)
        disk = targetFDS.disk
        diskSide = targetDiskSide
        diskPointer = 0

  inline private def checkIRQ(): Unit =
    irqHandler((irqTranferEnabled && irqTransferOccurred) || (diskIORegistersEnabled && irqTimerOccurred && irqTimerEnabled))

  override protected def writeCPU(address:Int,value:Int) : Unit =
    openbus = value
    if address >= 0x4040 && address <= 0x408A then
      audio.write(address,value)
    else if address > 0x5FFF && address < 0xE000 then
      ram(address & 0x7FFF) = value
    else
      address match
        case 0x4020 => // IRQ reload value low
          irqTimerLatch = (irqTimerLatch & 0xFF00) | value
        case 0x4021 => // IRQ reload value high
          irqTimerLatch = (irqTimerLatch & 0xFF) | value << 8
        case 0x4022 if diskIORegistersEnabled => // IRQ control
          irqTimerRepeatFlag = (value & 1) > 0
          irqTimerEnabled = (value & 2) > 0
          if irqTimerEnabled then
            irqTimerCounter = irqTimerLatch
          checkIRQ()
        case 0x4023 => // Master I/O enable
          diskIORegistersEnabled = (value & 1) > 0
          audio.setSoundIOEnabled((value & 2) > 0)
          checkIRQ()
        case 0x4024 => // Write Data Register
          writeData(value)
        case 0x4025 => // FDS Control
          writeFDSControl(value)
        case _ =>

  override protected def readCPU(address:Int) : Int =
    openbus =
      if address > 0xDFFF then
        bios(address & 0x1FFF)
      else if address > 0x5FFF then
        ram(address & 0x7FFF)
      else
        address match
          case 0x4030 => readStatusRegister()
          case 0x4031 => readDataRegister()
          case 0x4032 => readDiskDriveStatus()
          case 0x4033 => externalConnectorRead()
          case _ =>
            audio.read(address)
    openbus

  inline private def writeData(value:Int): Unit =
    if isDiskInserted() && !readMode && diskIORegistersEnabled && diskPointer >= 0 && diskPointer < disk(diskSide).length then
      if writeSkip > 0 then
        writeSkip -= 1
      else if diskPointer >= 2 then
        disk(diskSide)(diskPointer - 2) = value
        written = true
        setLedOn(true)
        //println("FDS Writing side=" + diskSide + " head=" + (diskPointer - 2) + " = " + value)

  inline private def writeFDSControl(value:Int): Unit =
    irqTransferOccurred = false
    checkIRQ()
    /*
      (O) -stop motor
      ---------------
      Applicable mostly to the FDS disk drive unit only, the falling edge of this
      signal would instruct the drive to stop the current scan of the disk.
    */
    motorOn = (value & 1) == 1
    /*
      (O) -scan media
      ---------------
      While inactive, this instructs the storage media pointer to be reset (and
      stay reset) at the beginning of the media. When active, the media pointer is
      to be advanced at a constant rate, and data progressively transferred
      to/from the media (via the media pointer).
    */
    val preScanningMedia = scanningMedia
    scanningMedia = (value & 2) > 0
    readMode = (value & 4) > 0
    setMirroring(if (value & 8) == 0 then Cartridge.Mirroring.VERTICAL else Cartridge.Mirroring.HORIZONTAL)
    /*
      CRC control. ROM BIOS subroutines set this bit while processing the CRC
      data at the end of a block. While it is unclear why this bit is set during
      block reading, when this bit is set during a block write, this instructs the
      2C33 to pipe the current contents of the CRC register to the disk (data in
      $4024 is effectively ignored during this time).
    */
    crcControl = (value & 0x10) > 0
    /*
      This bit is typically set while the disk head is in a GAP period on the
      disk. When this is done, it issues a reset to the 2C33's internal CRC
      accumulator.

      During reads, setting this bit instructs the 2C33 to wait for the first set
      bit (block start mark) to be read off the disk, before accumulating any
      serial data in the FDS's internal shift registers, and setting the byte
      transfer ready flag for the first time (and then every 8-bit subsequent
      transfer afterwards).

      During writes, setting this bit instructs the 2C33 to immediately load the
      contents of $4024 into a shift register, set the byte transfer flag, start
      writing the data from the shift register onto the disk, and repeat this
      process on subsequent 8-bit transfers. While this bit is 0, data in $4024 is
      ignored, and a stream of 0's is written to the disk instead.
    */
    val preIsOnGap = isOnGAP
    isOnGAP = (value & 0x40) > 0
    /*
      When set, generates an IRQ when the byte transfer flag raises.
    */
    irqTranferEnabled = (value & 0x80) > 0

    if isDiskInserted() then
      if !isOnGAP then
        if preIsOnGap && !crcControl then
          irqDiskSeek = 200
          diskPointer -= 2
        if diskPointer < 0 then diskPointer = 0
      else
        irqDiskSeek = 200

      if scanningMedia then
        diskPointer = 0
        irqDiskSeek = 200

      if !readMode then
        writeSkip = 2

    //println(s"writeFDSContro: motorOn=$motorOn scanningMedia=$scanningMedia crcControl=$crcControl isOnGAP=$isOnGAP irqTrasferEnabled=$irqTranferEnabled")

  /**
    7  bit  0
    ---------
    IExB xxTD
    || |   ||
    || |   |+- Timer Interrupt (1: an IRQ occurred)
    || |   +-- Byte transfer flag. Set every time 8 bits have been transfered between the RAM adaptor & disk drive (service $4024/$4031).
    || |       Reset when $4024, $4031, or $4030 has been serviced.
    || +------ CRC control (0: CRC passed; 1: CRC error)
    |+-------- End of Head (1 when disk head is on the most inner track)
    +--------- Disk Data Read/Write Enable (1 when disk is readable/writable)
   */
  inline private def readStatusRegister(): Int =
    var status = 0
    if irqTimerOccurred then status |= 1
    if irqTransferOccurred then status |= 2
    // TODO End of Head
    irqTimerOccurred = false
    irqTransferOccurred = false
    checkIRQ()
    status

  inline private def readDataRegister(): Int =
    var value = 0
    if isDiskInserted() then
      value = disk(diskSide)(diskPointer)
      if diskPointer < disk(diskSide).length then
        diskPointer += 1
      irqDiskSeek = 150
      irqTransferOccurred = false
      checkIRQ()
      setLedOn(true)
    //println(s"READ: $value '${value.toChar}' head=$diskPointer")
    value

  /**
    7  bit  0
    ---------
    xxxx xPRS
          |||
          ||+- Disk flag  (0: Disk inserted; 1: Disk not inserted)
          |+-- Ready flag (0: Disk readу; 1: Disk not ready)
          +--- Protect flag (0: Not write protected; 1: Write protected or disk ejected)
   */
  inline private def readDiskDriveStatus(): Int =
    var status = 0
    fds match
      case Some(_) =>
        /*
          (I) -ready
          ----------
          Applicable mostly to the FDS disk drive unit only, the falling edge of this
          signal would indicate to the RAM adaptor that the disk drive has
          acknowledged the "-scan media" signal, and the disk drive head is currently
          at the beginning of the disk (most outer track). While this signal remains
          active, this indicates that the disk head is advancing across the disk's
          surface, and apropriate data can be transferred to/from the disk. This
          signal would then go inactive if the head advances to the end of the disk
          (most inner track), or the "-scan media" signal goes inactive.
        */
        //if !readyFlag then status |= 2
        if !motorOn || scanningMedia then status |= 2
      case None =>
        status = 7
    status

  /**
    7  bit  0
    ---------
    BIII IIII
    |||| ||||
    |+++-++++- Input from expansion terminal where there's a shutter on the back of the ram card.
    +--------- Battery status (0: Voltage is low; 1: Good).
   */
  inline private def externalConnectorRead(): Int =
    0x80