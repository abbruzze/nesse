package ucesoft.nes.sound
import java.io.{ObjectInputStream, ObjectOutputStream}

object Pulse:
  val DUTY_SEQUENCE = Array(
    Array(0,1,0,0,0,0,0,0), // 12.5%
    Array(0,1,1,0,0,0,0,0), // 25%
    Array(0,1,1,1,1,0,0,0), // 50%
    Array(1,0,0,1,1,1,1,1)  // 25& negated
  )
/**
 *
                     +---------+    +---------+
                     |  Sweep  |--->|Timer / 2|
                     +---------+    +---------+
                          |              |
                          |              v
                          |         +---------+    +---------+
                          |         |Sequencer|    | Length  |
                          |         +---------+    +---------+
                          |              |              |
                          v              v              v
      +---------+        |\             |\             |\          +---------+
      |Envelope |------->| >----------->| >----------->| >-------->|   DAC   |
      +---------+        |/             |/             |/          +---------+

  There are two square channels beginning at registers $4000 and $4004. Each
  contains the following: Envelope Generator, Sweep Unit, Timer with
  divide-by-two on the output, 8-step sequencer, Length Counter.

  $4000/$4004: duty, envelope
  $4001/$4005: sweep unit
  $4002/$4006: period low
  $4003/$4007: reload length counter, period high

  In addition to the envelope, the first register controls the duty cycle of the
  square wave, without resetting the position of the sequencer:

      dd-- ----       duty cycle select

      d   waveform sequence
      ---------------------
           _       1
      0   - ------ 0 (12.5%)

           __      1
      1   -  ----- 0 (25%)

           ____    1
      2   -    --- 0 (50%)

          _  _____ 1
      3    --      0 (25% negated)


  When the fourth register is written to, the sequencer is restarted.

  The sequencer is clocked by the divided timer output.

  When the sequencer output is low, the DAC receives 0.

 */
class Pulse(id:Int) extends Channel:
  import Pulse.*

  private val envelop = new Envelope
  private val sequencer = new Sequencer(DUTY_SEQUENCE(0))
  private val timer = new Divider(() => sequencer.step() )
  private val sweep = new Sweep(onSweep)
  private val lengthCounter = new LengthCounter
  private var currentPeriod = 0
  private var targetPeriod = 0
  private var evenCycle = false

  override def reset(): Unit =
    envelop.reset()
    sequencer.reset()
    timer.reset()
    sweep.reset()
    lengthCounter.reset()
    currentPeriod = 0
    targetPeriod = 0
    evenCycle = false

  private def onSweep() : Unit =
    targetPeriod = sweep.calculateTargetPeriod(currentPeriod, id == 1)
    if targetPeriod < 0 then
      targetPeriod = 0
    if targetPeriod < 0x800 then
      currentPeriod = targetPeriod
    if sweep.getShiftCount() != 0 then
      timer.setPeriod(currentPeriod)

  override def clock() : Unit =
    evenCycle = !evenCycle
    if evenCycle then
      timer.clock()

  override def clockLengthCounterAndSweep(): Unit =
    lengthCounter.clock()
    sweep.clock()

  override def clockEnvelopAndLinearCounter(): Unit =
    envelop.clock()

  override def output(): Int =
    if sequencer.output == 0 ||
      lengthCounter.getLengthCounter() == 0 ||
      targetPeriod > 0x7FF ||
      timer.getValue() < 8 then 0
    else envelop.output()

  override def setEnabled(enabled: Boolean): Unit =
    lengthCounter.setEnabled(enabled)

  override def setRegister(index: Int, value: Int): Unit =
    index match
      case 0 =>
        envelop.setRegister(value)
        lengthCounter.setHalt(envelop.getLoopFlag())
        sequencer.setSequence(DUTY_SEQUENCE((value >> 6) & 3))
      case 1 =>
        sweep.setRegister(value)
      case 2 =>
        currentPeriod = (currentPeriod & ~0xFF) | value
        targetPeriod = sweep.calculateTargetPeriod(currentPeriod, id == 1)
        timer.setPeriod(currentPeriod)
      case 3 =>
        currentPeriod = (currentPeriod & 0xFF) | ((value & 7) << 8)
        targetPeriod = sweep.calculateTargetPeriod(currentPeriod, id == 1)
        timer.setPeriod(currentPeriod)
        lengthCounter.setRegister(value)
        sequencer.reset()
        envelop.setStartFlag()

  override def isActive(): Boolean = lengthCounter.getLengthCounter() > 0

  override def saveState(out: ObjectOutputStream): Unit =
    envelop.saveState(out)
    sequencer.saveState(out)
    timer.saveState(out)
    sweep.saveState(out)
    lengthCounter.saveState(out)
    out.writeInt(currentPeriod)
    out.writeInt(targetPeriod)
    out.writeBoolean(evenCycle)

  override def loadState(in: ObjectInputStream): Unit =
    envelop.loadState(in)
    sequencer.loadState(in)
    timer.loadState(in)
    sweep.loadState(in)
    lengthCounter.loadState(in)
    currentPeriod = in.readInt()
    targetPeriod = in.readInt()
    evenCycle = in.readBoolean()

