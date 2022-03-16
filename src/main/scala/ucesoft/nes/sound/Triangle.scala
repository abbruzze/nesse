package ucesoft.nes.sound
import java.io.{ObjectInputStream, ObjectOutputStream}

object Triangle:
  val SEQUENCE = ((0xF to 0 by -1) ++ (0 to 0xF)).toArray
/**
 *
                     +---------+    +---------+
                     |LinearCtr|    | Length  |
                     +---------+    +---------+
                          |              |
                          v              v
      +---------+        |\             |\         +---------+    +---------+
      |  Timer  |------->| >----------->| >------->|Sequencer|--->|   DAC   |
      +---------+        |/             |/         +---------+    +---------+

  The triangle channel contains the following: Timer, 32-step sequencer, Length
  Counter, Linear Counter, 4-bit DAC.

  $4008: length counter disable, linear counter
  $400A: period low
  $400B: length counter reload, period high

  When the timer generates a clock and the Length Counter and Linear Counter both
  have a non-zero count, the sequencer is clocked.

  The sequencer feeds the following repeating 32-step sequence to the DAC:

      F E D C B A 9 8 7 6 5 4 3 2 1 0 0 1 2 3 4 5 6 7 8 9 A B C D E F

  At the lowest two periods ($400B = 0 and $400A = 0 or 1), the resulting
  frequency is so high that the DAC effectively outputs a value half way between
  7 and 8.
 */
class Triangle extends Channel:
  import Triangle.*

  private val timer = new Divider(divClock)
  private val lengthCounter = new LengthCounter
  private val linearCounter = new Divider( () => {})
  private var reloadFlag = false
  private var controlFlag = false
  private val sequencer = new Sequencer(SEQUENCE)

  private var counterReload = 0
  private var timerPeriod = 0

  override def reset(): Unit =
    timer.reset()
    lengthCounter.reset()
    linearCounter.reset()
    reloadFlag = false
    controlFlag = false
    sequencer.reset()
    counterReload = 0
    timerPeriod = 0

  private def divClock() : Unit =
    if linearCounter.getValue() > 0 && lengthCounter.getLengthCounter() > 0 then
      sequencer.step()

  override def clock(): Unit =
    timer.clock()

  override def clockEnvelopAndLinearCounter(): Unit =
    if reloadFlag then
      linearCounter.resetCounter()
    else
      linearCounter.clock()
    if !controlFlag then
      reloadFlag = false

  override def clockLengthCounterAndSweep(): Unit =
    lengthCounter.clock()

  override def setEnabled(enabled: Boolean): Unit =
    lengthCounter.setEnabled(enabled)

  override def setRegister(index: Int, value: Int): Unit =
    index match
      case 0 =>
        counterReload = value & 0x7F
        controlFlag = (value & 0x80) != 0
        lengthCounter.setHalt(controlFlag)
        linearCounter.setPeriod(counterReload)
      case 2 =>
        timerPeriod = (timerPeriod & ~0xFF) | value
        timer.setPeriod(timerPeriod)
      case 3 =>
        timerPeriod = (timerPeriod & 0xFF) | ((value & 7) << 8)
        timer.setPeriod(timerPeriod)
        lengthCounter.setRegister(value)
        reloadFlag = true
      case _ =>

  override def output(): Int =
    if lengthCounter.getLengthCounter() == 0 ||
        linearCounter.getValue() == 0 || timerPeriod <= 1 then 0 else sequencer.output

  override def isActive(): Boolean = lengthCounter.getLengthCounter() > 0 && linearCounter.getValue() > 0

  override def saveState(out: ObjectOutputStream): Unit =
    timer.saveState(out)
    lengthCounter.saveState(out)
    linearCounter.saveState(out)
    sequencer.saveState(out)
    out.writeBoolean(reloadFlag)
    out.writeBoolean(controlFlag)
    out.writeInt(counterReload)
    out.writeInt(timerPeriod)

  override def loadState(in: ObjectInputStream): Unit =
    timer.loadState(in)
    lengthCounter.loadState(in)
    linearCounter.loadState(in)
    sequencer.loadState(in)
    reloadFlag = in.readBoolean()
    controlFlag = in.readBoolean()
    counterReload = in.readInt()
    timerPeriod = in.readInt()