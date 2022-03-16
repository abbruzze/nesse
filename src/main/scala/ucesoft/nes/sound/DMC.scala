package ucesoft.nes.sound

import ucesoft.nes.Cartridge
import ucesoft.nes.cpu.Memory

import java.io.{ObjectInputStream, ObjectOutputStream}

object DMC:
  val LOOKUP_TABLE_NTSC = Array(
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54
  )
  val LOOKUP_TABLE_PAL = Array(
    398, 354, 316, 298, 276, 236, 210, 198, 176, 148, 132, 118, 98, 78, 66, 50
  )

/**
 *
    +----------+    +---------+
    |DMA Reader|    |  Timer  |
    +----------+    +---------+
         |               |
         |               v
    +----------+    +---------+     +---------+     +---------+
    |  Buffer  |----| Output  |---->| Counter |---->|   DAC   |
    +----------+    +---------+     +---------+     +---------+

  The DMC can output samples composed of 1-bit deltas and its DAC can be directly
  changed. It contains the following: DMA reader, interrupt flag, sample buffer,
  Timer, output unit, 7-bit counter tied to 7-bit DAC.

  $4010: mode, frequency
  $4011: DAC
  $4012: address
  $4013: length

  On power-up, the DAC counter contains 0.

  Register $4010 sets the interrupt enable, loop, and timer period. If the new
  interrupt enabled status is clear, the interrupt flag is cleared.

      il-- ffff       interrupt enabled, loop, frequency index

      f   period
      ----------
      0   $1AC
      1   $17C
      2   $154
      3   $140
      4   $11E
      5   $0FE
      6   $0E2
      7   $0D6
      8   $0BE
      9   $0A0
      A   $08E
      B   $080
      C   $06A
      D   $054
      E   $048
      F   $036

  A write to $4011 sets the counter and DAC to a new value:

      -ddd dddd       new DAC value

  Sample Buffer
  -------------
  The sample buffer either holds a single sample byte or is empty. It is filled
  by the DMA reader and can only be emptied by the output unit, so once loaded
  with a sample it will be eventually output.

  DMA Reader
  ----------
  The DMA reader fills the sample buffer with successive bytes from the current
  sample, whenever it becomes empty. It has an address counter and a bytes remain
  counter.

  When the DMC sample is restarted, the address counter is set to register $4012
   * $40 + $C000 and the bytes counter is set to register $4013 * $10 + 1.

  When the sample buffer is in an empty state and the bytes counter is non-zero,
  the following occur: The sample buffer is filled with the next sample byte read
  from memory at the current address, subject to whatever mapping hardware is
  present (the same as CPU memory accesses). The address is incremented; if it
  exceeds $FFFF, it is wrapped around to $8000. The bytes counter is decremented;
  if it becomes zero and the loop flag is set, the sample is restarted (see
  above), otherwise if the bytes counter becomes zero and the interrupt enabled
  flag is set, the interrupt flag is set.

  When the DMA reader accesses a byte of memory, the CPU is suspended for 4 clock
  cycles.

  Output Unit
  -----------
  The output unit continually outputs complete sample bytes or silences of equal
  duration. It contains an 8-bit right shift register, a counter, and a silence
  flag.

  When an output cycle is started, the counter is loaded with 8 and if the sample
  buffer is empty, the silence flag is set, otherwise the silence flag is cleared
  and the sample buffer is emptied into the shift register.

  On the arrival of a clock from the timer, the following actions occur in order:

      1. If the silence flag is clear, bit 0 of the shift register is applied to
  the DAC counter: If bit 0 is clear and the counter is greater than 1, the
  counter is decremented by 2, otherwise if bit 0 is set and the counter is less
  than 126, the counter is incremented by 2.

      1) The shift register is clocked.

      2) The counter is decremented. If it becomes zero, a new cycle is started.
 */
class DMC(irqLow:Boolean => Unit,dmaReadSampleHandler : Int => Int) extends Channel with Clocked:
  import DMC.*

  private val timer = new Divider(divClock)
  private var interruptFlag = false
  private var irqEnabled = false
  private var loopFlag = false
  private var period = 0
  private var sampleAddress = 0
  private var sampleLength = 0

  private var currentAddress = 0
  private var bytesRemaining = 0
  private var sampleBuffer = 0
  private var sampleBufferFilled = false
  private var requestedSample = false

  private var silenceFlag = false
  private var shiftRegister = 0
  private var bitsRemainingCounter = 0
  private var outputLevel = 0

  private var LOOKUP_TABLE : Array[Int] = LOOKUP_TABLE_NTSC
  private var tv : Cartridge.TV = Cartridge.TV.NTSC

  override def setTV(tv: Cartridge.TV): Unit =
    import ucesoft.nes.Cartridge.TV.*

    this.tv = tv
    LOOKUP_TABLE = tv match
      case NTSC => LOOKUP_TABLE_NTSC
      case PAL => LOOKUP_TABLE_PAL

  override def reset(): Unit =
    timer.reset()
    interruptFlag = false
    irqEnabled = false
    loopFlag = false
    period = 0
    sampleAddress = 0
    sampleLength = 0
    currentAddress = 0
    bytesRemaining = 0
    sampleBufferFilled = false
    requestedSample = false
    sampleBuffer = 0
    silenceFlag = false
    shiftRegister = 0
    bitsRemainingCounter = 0
    outputLevel = 0

  private def divClock() : Unit =
    if !silenceFlag then
      val bit0 = shiftRegister & 1
      if bit0 == 1 && outputLevel < 126 then outputLevel += 2
      else
        if bit0 == 0 && outputLevel > 1 then outputLevel -= 2
      shiftRegister >>= 1

    if bitsRemainingCounter == 0 then
      bitsRemainingCounter = 8
      if sampleBufferFilled then
        silenceFlag = false
        sampleBufferFilled = false
        shiftRegister = sampleBuffer
      else
        silenceFlag = true

    bitsRemainingCounter -= 1

  def setSample(sample:Int): Unit =
    sampleBuffer = sample
    sampleBufferFilled = true
    requestedSample = false

    currentAddress += 1
    if currentAddress == 0x10000 then
      currentAddress = 0x8000

    if bytesRemaining == 0 then
      if loopFlag then
        currentAddress = sampleAddress
        bytesRemaining = sampleLength
      else
        if irqEnabled then
          interruptFlag = true
          irqLow(true)

  override def clock(): Unit =
    timer.clock()

    if !requestedSample && !sampleBufferFilled && bytesRemaining > 0 then
      requestedSample = true
      dmaReadSampleHandler(currentAddress)
      bytesRemaining -= 1


  override def setEnabled(enabled: Boolean): Unit =
    interruptFlag = false
    irqLow(false)
    if enabled then
      if bytesRemaining == 0 then
        currentAddress = sampleAddress
        bytesRemaining = sampleLength
    else
      bytesRemaining = 0

  override def setRegister(index: Int, value: Int): Unit =
    index match
      case 0 =>
        setIRQEnabled((value & 0x80) != 0)
        loopFlag = (value & 0x40) != 0
        period = LOOKUP_TABLE(value & 0xF)
        timer.setPeriod(period)
      case 1 =>
        outputLevel = value & 0x7F
      case 2 =>
        sampleAddress = 0xC000 | ((value & 0xFF) << 6)
      case 3 =>
        sampleLength = ((value & 0xFF) << 4) | 1
      case _ =>

  override def output(): Int = outputLevel

  override def isActive(): Boolean = bytesRemaining > 0

  private def setIRQEnabled(enabled: Boolean): Unit =
    irqEnabled = enabled
    if (!irqEnabled)
      interruptFlag = false
      irqLow(false)

  override def getInterruptFlag(): Boolean = interruptFlag

  def clearInterruptFlag(): Unit =
    interruptFlag = false
    irqLow(false)

  override def clockLengthCounterAndSweep(): Unit = {}
  override def clockEnvelopAndLinearCounter(): Unit = {}

  override def saveState(out: ObjectOutputStream): Unit =
    timer.saveState(out)
    out.writeBoolean(interruptFlag)
    out.writeBoolean(irqEnabled)
    out.writeBoolean(loopFlag)
    out.writeInt(period)
    out.writeInt(sampleAddress)
    out.writeInt(sampleLength)
    out.writeInt(currentAddress)
    out.writeInt(bytesRemaining)
    out.writeBoolean(sampleBufferFilled)
    out.writeInt(sampleBuffer)
    out.writeBoolean(silenceFlag)
    out.writeInt(shiftRegister)
    out.writeInt(bitsRemainingCounter)
    out.writeInt(outputLevel)
    out.writeObject(tv)

  override def loadState(in: ObjectInputStream): Unit =
    timer.loadState(in)
    interruptFlag = in.readBoolean()
    irqEnabled = in.readBoolean()
    loopFlag = in.readBoolean()
    period = in.readInt()
    sampleAddress = in.readInt()
    sampleLength = in.readInt()
    currentAddress = in.readInt()
    bytesRemaining = in.readInt()
    sampleBufferFilled = in.readBoolean()
    sampleBuffer = in.readInt()
    silenceFlag = in.readBoolean()
    shiftRegister = in.readInt()
    bitsRemainingCounter = in.readInt()
    outputLevel = in.readInt()
    tv = in.readObject().asInstanceOf[Cartridge.TV]
    setTV(tv)