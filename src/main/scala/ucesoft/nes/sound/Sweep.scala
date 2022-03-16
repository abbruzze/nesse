package ucesoft.nes.sound

import java.io.{ObjectInputStream, ObjectOutputStream}

/**
 * The sweep unit can adjust a square channel's period periodically. It contains a
 * divider and a shifter.
 *
 * A channel's second register configures the sweep unit:
 *
 * eppp nsss       enable, period, negate, shift
 *
 * The divider's period is set to p + 1.
 *
 * The shifter continuously calculates a result based on the channel's period. The
 * channel's period (from the third and fourth registers) is first shifted right
 * by s bits. If negate is set, the shifted value's bits are inverted, and on the
 * second square channel, the inverted value is incremented by 1. The resulting
 * value is added with the channel's current period, yielding the final result.
 *
 * When the sweep unit is clocked, the divider is *first* clocked and then if
 * there was a write to the sweep register since the last sweep clock, the divider
 * is reset.
 *
 * When the channel's period is less than 8 or the result of the shifter is
 * greater than $7FF, the channel's DAC receives 0 and the sweep unit doesn't
 * change the channel's period. Otherwise, if the sweep unit is enabled and the
 * shift count is greater than 0, when the divider outputs a clock, the channel's
 * period in the third and fourth registers are updated with the result of the
 * shifter.
 */
class Sweep(onSweep: () => Unit) extends Clocked:
  private val divider = new Divider(divClock)
  private var reloadFlag = false

  private var enabled = false
  private var period = 0
  private var negate = false
  private var shiftCount = 0
  
  def reset(): Unit =
    divider.reset()
    reloadFlag = false
    enabled = false
    period = 0
    negate = false
    shiftCount = 0

  private def divClock() : Unit =
    if enabled then
      onSweep()

  def clock(): Unit =
    if reloadFlag then
      if enabled then divider.clock()
      divider.resetCounter()
      reloadFlag = false
    else divider.clock()

  def setRegister(value: Int): Unit =
    enabled = (value & 0x80) != 0
    period = (value & 0x70) >> 4
    negate = (value & 8) != 0
    shiftCount = value & 7
    reloadFlag = true
    divider.setPeriod(period)

  def calculateTargetPeriod(currentPeriod: Int, useOnesComponent: Boolean): Int =
    var changeAmount = currentPeriod >> shiftCount
    if negate then
      if useOnesComponent then
        changeAmount = ~changeAmount
      else
        changeAmount = -changeAmount

    currentPeriod + changeAmount

  def getShiftCount(): Int = shiftCount

  def saveState(out: ObjectOutputStream) : Unit =
    divider.saveState(out)
    out.writeBoolean(reloadFlag)
    out.writeBoolean(enabled)
    out.writeInt(period)
    out.writeBoolean(negate)
    out.writeInt(shiftCount)

  def loadState(in:ObjectInputStream) : Unit =
    divider.loadState(in)
    reloadFlag = in.readBoolean()
    enabled = in.readBoolean()
    period = in.readInt()
    negate = in.readBoolean()
    shiftCount = in.readInt()
