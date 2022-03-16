package ucesoft.nes.sound

import java.io.{ObjectInputStream, ObjectOutputStream}

/**
 * An envelope generator can generate a constant volume or a saw envelope with
 * optional looping. It contains a divider and a counter.
 *
 * A channel's first register controls the envelope:
 *
 * --ld nnnn       loop, disable, n
 *
 * Note that the bit position for the loop flag is also mapped to a flag in the
 * Length Counter.
 *
 * The divider's period is set to n + 1.
 *
 * When clocked by the frame sequencer, one of two actions occurs: if there was a
 * write to the fourth channel register since the last clock, the counter is set
 * to 15 and the divider is reset, otherwise the divider is clocked.
 *
 * When the divider outputs a clock, one of two actions occurs: if loop is set and
 * counter is zero, it is set to 15, otherwise if counter is non-zero, it is
 * decremented.
 *
 * When disable is set, the channel's volume is n, otherwise it is the value in
 * the counter. Unless overridden by some other condition, the channel's DAC
 * receives the channel's volume value.
 */
class Envelope extends Clocked:
  private var startFlag = false
  private val divider = new Divider(divClock)
  private var decayLevel = 0

  private var isConstant = false
  private var volume = 0
  private var loopFlag = false
  
  def reset() : Unit =
    startFlag = false
    divider.reset()
    decayLevel = 0
    isConstant = false
    volume = 0
    loopFlag = false

  override def clock() : Unit =
    if !startFlag then
      divider.clock()
    else
      startFlag = false
      divider.resetCounter()
      decayLevel = 15

  private def divClock() : Unit =
    if decayLevel > 0 then
      decayLevel -= 1
    else
    if loopFlag then
      decayLevel = 15

  def output(): Int = if isConstant then volume else decayLevel

  def setRegister(value: Int): Unit =
    isConstant = (value & 0x10) != 0
    loopFlag = (value & 0x20) != 0
    volume = value & 0xF
    divider.setPeriod(volume)

  def setStartFlag(): Unit =
    startFlag = true

  def getLoopFlag(): Boolean = loopFlag

  def saveState(out: ObjectOutputStream) : Unit =
    divider.saveState(out)
    out.writeBoolean(startFlag)
    out.writeInt(decayLevel)
    out.writeBoolean(isConstant)
    out.writeInt(volume)
    out.writeBoolean(loopFlag)

  def loadState(in:ObjectInputStream) : Unit =
    divider.loadState(in)
    startFlag = in.readBoolean()
    decayLevel = in.readInt()
    isConstant = in.readBoolean()
    volume = in.readInt()
    loopFlag = in.readBoolean()