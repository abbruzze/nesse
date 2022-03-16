package ucesoft.nes.sound
import ucesoft.nes.Cartridge

import java.io.{ObjectInputStream, ObjectOutputStream}

object Noise:
  val LOOKUP_TABLE_NTSC = Array(
    0x004,
    0x008,
    0x010,
    0x020,
    0x040,
    0x060,
    0x080,
    0x0A0,
    0x0CA,
    0x0FE,
    0x17C,
    0x1FC,
    0x2FA,
    0x3F8,
    0x7F2,
    0xFE4
  )
  val LOOKUP_TABLE_PAL = Array(
    4, 8, 14, 30, 60, 88, 118, 148, 188, 236, 354, 472, 708, 944, 1890, 3778
  )

/**

      +---------+    +---------+    +---------+
      |  Timer  |--->| Random  |    | Length  |
      +---------+    +---------+    +---------+
                          |              |
                          v              v
      +---------+        |\             |\         +---------+
      |Envelope |------->| >----------->| >------->|   DAC   |
      +---------+        |/             |/         +---------+

  The noise channel starts at register $400C and contains the following: Length
  Counter, Envelope Generator, Timer, 15-bit right shift register with feedback,
  4-bit DAC.

  $400C: envelope
  $400E: mode, period
  $400F: reload length counter

  Register $400E sets the random generator mode and timer period based on a 4-bit
  index into a period table:

      m--- iiii       mode, period index

      i   timer period
      ----------------
      0     $004
      1     $008
      2     $010
      3     $020
      4     $040
      5     $060
      6     $080
      7     $0A0
      8     $0CA
      9     $0FE
      A     $17C
      B     $1FC
      C     $2FA
      D     $3F8
      E     $7F2
      F     $FE4

  The shift register is clocked by the timer and the vacated bit 14 is filled
  with the exclusive-OR of *pre-shifted* bits 0 and 1 (mode = 0) or bits 0 and 6
  (mode = 1), resulting in 32767-bit and 93-bit sequences, respectively.

  When bit 0 of the shift register is set, the DAC receives 0.

  On power-up, the shift register is loaded with the value 1.

 */  
class Noise extends Channel:
  import Noise.*

  private val envelop = new Envelope()
  private val timer = new Divider(divClock)
  private val lengthCounter = new LengthCounter

  private var mode = false
  private var period = 0
  private var feedbackRegister = 1

  private var evenCycle = false

  private var LOOKUP_TABLE : Array[Int] = LOOKUP_TABLE_NTSC
  private var tv : Cartridge.TV = Cartridge.TV.NTSC

  override def setTV(tv: Cartridge.TV): Unit =
    import ucesoft.nes.Cartridge.TV.*

    this.tv = tv
    LOOKUP_TABLE = tv match
      case NTSC => LOOKUP_TABLE_NTSC
      case PAL => LOOKUP_TABLE_PAL

  override def reset(): Unit =
    envelop.reset()
    timer.reset()
    lengthCounter.reset()
    mode = false
    period = 0
    feedbackRegister = 1
    evenCycle = false

  private def divClock() : Unit = {
    var feedback = feedbackRegister
    if mode then
      feedback ^= feedbackRegister >> 6
    else
      feedback ^= feedbackRegister >> 1

    feedbackRegister >>= 1
    feedbackRegister |= ((feedback & 1) << 14)
  }

  override def clock(): Unit =
    evenCycle = !evenCycle
    if evenCycle then
      timer.clock()

  override def clockLengthCounterAndSweep(): Unit =
    lengthCounter.clock()

  override def clockEnvelopAndLinearCounter(): Unit =
    envelop.clock()

  override def setEnabled(enabled: Boolean): Unit =
    lengthCounter.setEnabled(enabled)

  override def setRegister(index: Int, value: Int): Unit =
    index match
      case 0 =>
        envelop.setRegister(value)
        lengthCounter.setHalt(envelop.getLoopFlag())
      case 2 =>
        mode = (value & 0x80) != 0
        period = LOOKUP_TABLE(value & 0xF)
        timer.setPeriod(period)
      case 3 =>
        lengthCounter.setRegister(value)
        envelop.setStartFlag()
      case _ =>

  override def output() : Int =
    if (feedbackRegister & 1) != 0 ||
       lengthCounter.getLengthCounter() == 0 then 0 else envelop.output()

  override def isActive() : Boolean = lengthCounter.getLengthCounter() > 0

  override def saveState(out: ObjectOutputStream): Unit =
    envelop.saveState(out)
    timer.saveState(out)
    lengthCounter.saveState(out)
    out.writeBoolean(mode)
    out.writeInt(period)
    out.writeInt(feedbackRegister)
    out.writeBoolean(evenCycle)
    out.writeObject(tv)

  override def loadState(in: ObjectInputStream): Unit =
    envelop.loadState(in)
    timer.loadState(in)
    lengthCounter.loadState(in)
    mode = in.readBoolean()
    period = in.readInt()
    feedbackRegister = in.readInt()
    evenCycle = in.readBoolean()
    tv = in.readObject().asInstanceOf[Cartridge.TV]
    setTV(tv)