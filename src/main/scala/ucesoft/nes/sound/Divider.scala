package ucesoft.nes.sound

import java.io.{ObjectInputStream, ObjectOutputStream}

/**
 * A divider outputs a clock every n input clocks, where n is the divider's
 * period. It contains a counter which is decremented on the arrival of each
 * clock. When it reaches 0, it is reloaded with the period and an output clock is
 * generated. Resetting a divider reloads its counter without generating an output
 * clock. Changing a divider's period doesn't affect its current count.
 */
class Divider(outputClock: () => Unit) extends Clocked:
  private var period = 0
  private var counter = 0
  
  def reset() : Unit =
    period = 0
    resetCounter()

  def setPeriod(value: Int): Unit =
    period = value

  def resetCounter(): Unit =
    counter = period

  override def clock() : Unit =
    if counter == 0 then
      resetCounter()
      outputClock()
    else
      counter -= 1

  def getValue(): Int =
    counter
    
  def saveState(out: ObjectOutputStream) : Unit =
    out.writeInt(period)
    out.writeInt(counter)
    
  def loadState(in:ObjectInputStream) : Unit =
    period = in.readInt()
    counter = in.readInt()