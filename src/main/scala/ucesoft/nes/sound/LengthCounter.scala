package ucesoft.nes.sound

import java.io.{ObjectInputStream, ObjectOutputStream}

object LengthCounter:
  private val LOOKUP_TABLE = Array(
    0x0A, 0xFE,
    0x14, 0x02,
    0x28, 0x04,
    0x50, 0x06,
    0xA0, 0x08,
    0x3C, 0x0A,
    0x0E, 0x0C,
    0x1A, 0x0E,
    0x0C, 0x10,
    0x18, 0x12,
    0x30, 0x14,
    0x60, 0x16,
    0xC0, 0x18,
    0x48, 0x1A,
    0x10, 0x1C,
    0x20, 0x1E
  )
/**
 * A length counter allows automatic duration control. Counting can be halted and
  the counter can be disabled by clearing the appropriate bit in the status
  register, which immediately sets the counter to 0 and keeps it there.

  The halt flag is in the channel's first register. For the square and noise
  channels, it is bit 5, and for the triangle, bit 7:

      --h- ----       halt (noise and square channels)
      h--- ----       halt (triangle channel)

  Note that the bit position for the halt flag is also mapped to another flag in
  the Length Counter (noise and square) or Linear Counter (triangle).

  Unless disabled, a write the channel's fourth register immediately reloads the
  counter with the value from a lookup table, based on the index formed by the
  upper 5 bits:

      iiii i---       length index

      bits  bit 3
      7-4   0   1
          -------
      0   $0A $FE
      1   $14 $02
      2   $28 $04
      3   $50 $06
      4   $A0 $08
      5   $3C $0A
      6   $0E $0C
      7   $1A $0E
      8   $0C $10
      9   $18 $12
      A   $30 $14
      B   $60 $16
      C   $C0 $18
      D   $48 $1A
      E   $10 $1C
      F   $20 $1E

  See the clarifications section for a possible explanation for the values left
  column of the table.

  When clocked by the frame sequencer, if the halt flag is clear and the counter
  is non-zero, it is decremented.
 */
class LengthCounter extends Clocked:
  import LengthCounter.*
  
  private var halt = false
  private var lengthCounter = 0
  private var enabled = false
  
  def reset() : Unit =
    halt = false
    lengthCounter = 0
    enabled = false

  def setEnabled(enabled: Boolean): Unit =
    this.enabled = enabled
    if !enabled then
      this.lengthCounter = 0

  def setHalt(halt: Boolean): Unit = {
    this.halt = halt
  }

  def setRegister(value: Int): Unit =
    if enabled then 
      lengthCounter = LOOKUP_TABLE((value & 0xF8) >> 3)

  override def clock(): Unit =
    if !halt && lengthCounter > 0 then
      lengthCounter -= 1

  def getLengthCounter(): Int = lengthCounter

  def saveState(out: ObjectOutputStream) : Unit =
    out.writeBoolean(halt)
    out.writeInt(lengthCounter)
    out.writeBoolean(enabled)
    
  def loadState(in:ObjectInputStream) : Unit =
    halt = in.readBoolean()
    lengthCounter = in.readInt()
    enabled = in.readBoolean()
  
