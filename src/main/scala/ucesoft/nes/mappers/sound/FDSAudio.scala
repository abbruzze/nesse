package ucesoft.nes.mappers.sound

import ucesoft.nes.{Cartridge, NESComponent}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Arrays

class FDSAudio:
  private[this] final val MOD_ADJUSTMENTS = Array(0, 1, 2, 4, 0, -4, -2, -1)
  private[this] inline val MAX_OUTPUT = 32 * 63
  private[this] final val MASTER_VOLUMES = Array(1.0 / MAX_OUTPUT, 2.0 / (3.0 * MAX_OUTPUT),2.0 / (5.0 * MAX_OUTPUT))

  private[this] final val modTable = Array.ofDim[Int](64)
  private[this] final val volumeTable = Array.ofDim[Int](64)

  private[this] var masterVolume = 0.0
  private[this] var audioSample = 0.0
  private[this] var volumeOutput = 0
  private[this] var volumeSpeed = 0
  private[this] var volumeGain = 0
  private[this] var modSpeed = 0
  private[this] var modGain = 0
  private[this] var volumeFrequency = 0
  private[this] var modCounter = 0
  private[this] var modFrequency = 0
  private[this] var envelopeSpeed = 0
  private[this] var volumeOutputPosition = 0
  private[this] var volumeTimer = 0
  private[this] var modOutputPosition = 0
  private[this] var modTimer = 0
  private[this] var modAccumulator = 0
  private[this] var volumeAccumulator = 0
  private[this] var volumeAndSweepEnabled = false
  private[this] var volumeEnabled = false
  private[this] var volumeIncrease = false
  private[this] var volumeGainEnabled = false
  private[this] var modIncrease = false
  private[this] var modGainEnabled = false
  private[this] var volumeTableWriteEnabled = false
  private[this] var modEnabled = false
  private[this] var soundIOEnabled = false
  
  def saveState(out:ObjectOutputStream): Unit =
    out.writeObject(modTable)
    out.writeObject(volumeTable)
    out.writeDouble(masterVolume)
    out.writeDouble(audioSample)
    out.writeInt(volumeOutput)
    out.writeInt(volumeSpeed)
    out.writeInt(volumeGain)
    out.writeInt(modSpeed)
    out.writeInt(modGain)
    out.writeInt(volumeFrequency)
    out.writeInt(modCounter)
    out.writeInt(modFrequency)
    out.writeInt(envelopeSpeed)
    out.writeInt(volumeOutputPosition)
    out.writeInt(volumeTimer)
    out.writeInt(modOutputPosition)
    out.writeInt(modTimer)
    out.writeInt(modAccumulator)
    out.writeInt(volumeAccumulator)
    out.writeBoolean(volumeAndSweepEnabled)
    out.writeBoolean(volumeEnabled)
    out.writeBoolean(volumeIncrease)
    out.writeBoolean(volumeGainEnabled)
    out.writeBoolean(modIncrease)
    out.writeBoolean(modGainEnabled)
    out.writeBoolean(volumeTableWriteEnabled)
    out.writeBoolean(modEnabled)
    out.writeBoolean(soundIOEnabled)
    
  def loadState(in:ObjectInputStream): Unit =
    NESComponent.loadMemory(modTable,in)
    NESComponent.loadMemory(volumeTable,in)
    masterVolume = in.readDouble()
    audioSample = in.readDouble()
    volumeOutput = in.readInt()
    volumeSpeed = in.readInt()
    volumeGain = in.readInt()
    modSpeed = in.readInt()
    modGain = in.readInt()
    volumeFrequency = in.readInt()
    modCounter = in.readInt()
    modFrequency = in.readInt()
    envelopeSpeed = in.readInt()
    volumeOutputPosition = in.readInt()
    volumeTimer = in.readInt()
    modOutputPosition = in.readInt()
    modTimer = in.readInt()
    modAccumulator = in.readInt()
    volumeAccumulator = in.readInt()
    volumeAndSweepEnabled = in.readBoolean()
    volumeEnabled = in.readBoolean()
    volumeIncrease = in.readBoolean()
    volumeGainEnabled = in.readBoolean()
    modIncrease = in.readBoolean()
    modGainEnabled = in.readBoolean()
    volumeTableWriteEnabled = in.readBoolean()
    modEnabled = in.readBoolean()
    soundIOEnabled = in.readBoolean()

  def reset(): Unit =
    masterVolume = 0f
    audioSample = 0f
    volumeOutput = 0
    volumeSpeed = 0
    volumeGain = 0
    modSpeed = 0
    modGain = 0
    volumeFrequency = 0
    modCounter = 0
    modFrequency = 0
    envelopeSpeed = 0
    volumeOutputPosition = 0
    volumeTimer = 0
    modOutputPosition = 0
    modTimer = 0
    modAccumulator = 0
    volumeAccumulator = 0
    volumeAndSweepEnabled = false
    volumeEnabled = false
    volumeIncrease = false
    volumeGainEnabled = false
    modIncrease = false
    modGainEnabled = false
    volumeTableWriteEnabled = false
    modEnabled = false
    soundIOEnabled = false

    Arrays.fill(modTable, 0)
    Arrays.fill(volumeTable, 0)

  def setSoundIOEnabled(enabled:Boolean): Unit =
    soundIOEnabled = enabled
    if !soundIOEnabled then
      audioSample = 0

  def read(address:Int): Int =
    if (address & 0xFFC0) == 0x4040 then
      volumeTable(address & 0x3F)
    else address match
      case 0x4090 => // Volume gain ($4090)
        /*
          7  bit  0  (read; write through $4080)
          ---- ----
          OOVV VVVV
          |||| ||||
          ||++-++++- Current volume gain level
          ++-------- Returns 01 on read, likely from open bus
        */
        volumeGain | 0x40
      case 0x4091 => // Wave accumulator ($4091)
        /*
          7  bit  0  (read)
          ---- ----
          AAAA AAAA
          |||| ||||
          ++++-++++- Bits 12-19 of the wavetable address accumulator
        */
        (volumeAccumulator >> 12) & 0xFF
      case 0x4092 => // Mod gain ($4092)
        /*
          7  bit  0  (read; write through $4084)
          ---- ----
          OOVV VVVV
          |||| ||||
          ||++-++++- Current mod gain level
          ++-------- Returns 01 on read, likely from open bus
        */
        modGain | 0x40
      case 0x4093 => // Mod table address accumulator ($4093)
        /*
          7  bit  0  (read)
          ---- ----
          OAAA AAAA
          |||| ||||
          |+++-++++- Bits 5-11 of the modtable address accumulator
          +--------- Returns 0 on read, likely from open bus
        */
        (modAccumulator >> 5) & 0xFF
      case _ =>
        0

  def write(address:Int,value:Int): Unit =
    if (address & 0xFFC0) == 0x4040 then // Wavetable RAM ($4040-$407F)
      /*
        The 64-step waveform to be fed to the DAC. Each step consists of an unsigned sample value in the range [0, 63].
        However, it cannot be modified unless it is write-enabled, and it cannot be write-enabled while the sound is being played.
        When writing is disabled ($4089.7), reading anywhere in 4040-407F returns the value at the current wave position.
      */
      if volumeTableWriteEnabled then
        volumeTable(address & 0x003f) = value & 0x3F
    else address match
      case 0x4023 => // Master I/O enable ($4023)
        /*
          This register must be written to with bit 1 set for the sound registers to function.
          The FDS bios initializes this by writing $00 followed by $83 to it.
          7  bit  0
          ---------
          xxxx xxSD
                 ||
                 |+- Enable disk I/O registers
                 +-- Enable sound I/O registers
        */
        soundIOEnabled = (value & 2) > 0
      case 0x4080 => // Volume envelope ($4080)
        /*
          The envelope speed is set by this register whether or not the envelope is enabled by the high bit, but the current volume is set only if the high bit is set.
          The volume gain can range from 0 to 63; however, volume values above 32 are clamped to 32 before output.
          Changes to the volume envelope only take effect while the wavetable pointer (top 6 bits of wave accumulator) is 0. The volume envelope is basically a PWM unit, so apparently (wave_addr==0) is its comparator latch signal.
          Writing to this register immediately resets the clock timer that ticks the volume envelope (delaying the next tick slightly).
          7  bit  0  (write; read through $4090)
          ---- ----
          MDVV VVVV
          |||| ||||
          ||++-++++- (M=0) Volume envelope speed
          ||         (M=1) Volume gain and envelope speed.
          |+-------- Volume change direction (0: decrease; 1: increase)
          +--------- Volume envelope mode (0: on; 1: off)
        */
        volumeSpeed = value & 0x3F
        volumeIncrease = (value & 0x40) > 0
        volumeGainEnabled = (value & 0x80) == 0
        if !volumeGainEnabled then
          volumeGain = volumeSpeed
      case 0x4082 => // Frequency low ($4082)
        volumeFrequency = (volumeFrequency & 0xF00) | value
      case 0x4083 => // Frequency high ($4083)
        /*
          The high bit of this register halts the waveform and resets its phase to 0.
          Note that if halted it will output the constant value at $4040, and writes to the volume register $4080 or master volume $4089 will affect the output. The envelopes are not ticked while the waveform is halted.
          Bit 6 halts just the envelopes without halting the waveform, and also resets both of their timers.
          7  bit  0  (write)
          ---- ----
          MExx FFFF
          ||   ||||
          ||   ++++- Bits 8-11 of frequency
          |+-------- Disable volume and sweep envelopes (but not modulation)
          +--------- When enabled, envelopes run 4x faster. Also stops the mod table accumulator.
        */
        volumeFrequency = (volumeFrequency & 0x0FF) | ((value & 0x0F) << 8)
        volumeAndSweepEnabled = (value & 0x40) == 0
        volumeEnabled = (value & 0x80) == 0
        if !volumeEnabled then
          volumeAccumulator = 0
        if !volumeAndSweepEnabled then
          resetVolumeTimer()
          resetModTimer()
      case 0x4084 => // Mod envelope ($4084)
        /*
          The envelope speed is set by this register whether or not the envelope is enabled by the high bit, but the current mod gain is set only if the high bit is set.
          Writing to this register immediately resets the clock timer that ticks the modulator envelope (delaying the next tick slightly).
          7  bit  0  (write; read through $4092)
          ---- ----
          MDSS SSSS
          |||| ||||
          ||++-++++- (M=0) Mod envelope speed
          ||         (M=1) Mod gain and envelope speed.
          |+-------- Mod envelope direction (0: decrease; 1: increase)
          +--------- Mod envelope mode (0: on; 1: off)
        */
        modSpeed = value & 0x3F
        modIncrease = (value & 0x40) > 0
        modGainEnabled = (value & 0x80) == 0
        resetModTimer()
        if !modGainEnabled then
          modGain = modSpeed
      case 0x4085 => // Mod counter ($4085)
        /*
          This directly sets the 7-bit signed modulator counter that is otherwise controlled by the mod unit.
          Because the current playback position of the modulator unit is generally hard to predict while active,
          it is bad practice to write $4085 unless the mod unit is disabled via $4087, because it will generally result in a detuned note.
          Bio Miracle Bokutte Upa does this, and it requires cycle-accurate timing to emulate correctly. Some emulators incorrectly treat $4085 as a phase-reset for the mod table, which will obviate this timing issue.
          It is generally good practice to write 0 to $4085 to reset the counter after writing the mod table via $4088.
          7  bit  0  (write)
          ---- ----
          xBBB BBBB
           ||| ||||
           +++-++++- Mod counter (7-bit signed; minimum $40; maximum $3F)
        */
        modAccumulator = 0
        modCounter = ((value & 0x7F) << 25) >> 25 // TODO: ??
      case 0x4086 => // Mod frequency low ($4086)
        modFrequency = (modFrequency & 0xF00) | value
      case 0x4087 => // Mod frequency high ($4087)
        /*
          Setting the high bit of this register halts the mod unit, and allows the mod table to be written via $4088.
          Disabling the resets its timer accumulator, delaying the first tick after re-enabling.
          7  bit  0  (write)
          ---- ----
          HFxx FFFF
          ||   ||||
          ||   ++++- Bits 8-11 of modulation frequency
          |+-------- Force a carry out from bit 11 of mod accumulator. Step every clock.
          +--------- Halt mod table counter. (Freq mod table of modulation unit is always in effect.)
        */
        modFrequency = (modFrequency & 0x0FF) | ((value & 0x0F) << 8)
        modEnabled = (value & 0x80) == 0
        if !modEnabled then
          modAccumulator = 0
      case 0x4088 => // Mod table write ($4088)
        /*
          This register has no effect unless the mod unit is disabled via the high bit of $4087.
          The mod table is a ring buffer containing 32 entries.
          Writing to this register replaces an entry at the current mod table playback position with the written value, then advances the playback position to the following entry.
          The position of the mod table actually has 64 steps, but the least significant bit is not used to index the 32 entry table.
          Each entry will get applied twice as the mod table is stepped through.
          Writing to this register 32 times will effectively reset the phase of the mod table, having advanced the playback position back to its starting point.
          You should normally always write all 32 entries at once, since the starting write position is not easily predictable.
          Writing $4088 also increments the address (bits 13-17 of wave accumulator) when $4087.7=1.
          7  bit  0  (write)
          ---- ----
          xxxx xMMM
                |||
                +++- Modulation input
        */
        if !modEnabled then
          modTable((modOutputPosition + 1) & 0x3F) = value & 0x07
          modTable(modOutputPosition) = value & 0x07
          modOutputPosition = (modOutputPosition + 2) & 0x3F
      case 0x4089 => // Wave write / master volume ($4089)
        /*
          When the high bit is set, the current waveform output is held at its current level until the bit is cleared again.
          During this time, the wave unit will continue to run, even though the output level is held.
          7  bit  0  (write)
          ---- ----
          Wxxx xxVV
          |      ||
          |      ++- Master volume (0: full; 1: 2/3; 2: 2/4; 3: 2/5)
          |          Output volume = current volume (see $4080 above) * master volume
          +--------- Wavetable write enable
                     (0: write protect RAM; 1: write enable RAM and hold channel)
        */
        masterVolume = MASTER_VOLUMES(value & 0x03)
        volumeTableWriteEnabled = (value & 0x80) > 0
      case 0x408A => // Envelope speed ($408A)
        /*
          This sets a clock multiplier for the volume and modulator envelopes.
          Few FDS NSFs write to this register. The BIOS initializes this to $E8.
          7  bit  0  (write)
          ---- ----
          SSSS SSSS
          |||| ||||
          ++++-++++- Sets speed of volume envelope and sweep envelope
                     (0: disable them)
        */
        envelopeSpeed = value
        resetVolumeTimer()
        resetModTimer()
      case _ =>

  private inline def resetVolumeTimer(): Unit = volumeTimer = ((volumeSpeed + 1) << 3) * envelopeSpeed
  private inline def resetModTimer(): Unit = modTimer = ((modSpeed + 1) << 3) * envelopeSpeed
  private inline def tickVolumeUnit(): Unit =
    if volumeIncrease then
      if volumeGain < 32 then
        volumeGain += 1
    else if volumeGain > 0 then
        volumeGain -= 1
    resetVolumeTimer()

  private inline def tickModUnit(): Unit =
    if modIncrease then
      if modGain < 32 then
        modGain += 1
    else if modGain > 0 then
        modGain -= 1
    resetModTimer()


  final def clock(): Unit =
    if !soundIOEnabled then return

    if volumeAndSweepEnabled && volumeEnabled && envelopeSpeed != 0 then
      if volumeGainEnabled then
        volumeTimer -= 1
        if volumeTimer < 0 then
          tickVolumeUnit()
      if modGainEnabled then
        modTimer -= 1
        if modTimer < 0 then
          tickModUnit()

    if modEnabled then
      modAccumulator += modFrequency
      while modAccumulator >= 0x10000 do
        modAccumulator -= 0x10000
        val modValue = modTable(modOutputPosition)
        modOutputPosition = (modOutputPosition + 1) & 0x3F
        if modValue == 4 then
          modCounter = 0
        else
          modCounter = ((modCounter + MOD_ADJUSTMENTS(modValue)) << 25) >> 25

    if volumeEnabled then
      var modulatedPitch = 0
      if modGain != 0 then
        modulatedPitch = modCounter * modGain
        var remainder = modulatedPitch & 0x0F
        modulatedPitch >>= 4
        if remainder > 0 && (modulatedPitch & 0x80) == 0 then
          if modCounter < 0 then
            modulatedPitch -= 1
          else
            modulatedPitch += 2
        modulatedPitch = (((modulatedPitch + 64) & 0xFF) - 64) * volumeFrequency
        remainder = modulatedPitch & 0x3F
        modulatedPitch >>= 6
        if remainder >= 32 then
          modulatedPitch += 1
      else
          modulatedPitch = 0
      volumeAccumulator += volumeFrequency + modulatedPitch

      while volumeAccumulator >= 0x10000 do
        volumeAccumulator -= 0x10000
        volumeOutputPosition = (volumeOutputPosition + 1) & 0x3F

      if !volumeTableWriteEnabled then
        volumeOutput = volumeGain
        if (volumeOutput > 32) volumeOutput = 32
        volumeOutput *= volumeTable(volumeOutputPosition)

      audioSample = volumeOutput * masterVolume

  def getAudioSample(): Double = audioSample