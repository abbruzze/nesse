package ucesoft.nes.mappers.sound

import ucesoft.nes.NESComponent

import java.io.{ObjectInputStream, ObjectOutputStream}

class Namco163:
  private[this] val ram = Array.ofDim[Int](0x80)
  private[this] var soundEnabled = false
  private[this] var address = 0
  private[this] var addressAutoIncrement = false
  private[this] var enabledChannels = 0
  private[this] var cycles = 0
  private[this] val channelOutput = Array.ofDim[Int](8)
  private[this] var channelIndex = 0
  private[this] var sample = 0.0
  
  def reset(): Unit =
    java.util.Arrays.fill(ram,0)
    java.util.Arrays.fill(channelOutput,0)
    soundEnabled = false
    addressAutoIncrement = false
    enabledChannels = 0
    cycles = 0
    channelIndex = 0
    sample = 0.0

  def saveState(out:ObjectOutputStream): Unit =
    out.writeObject(ram)
    out.writeBoolean(soundEnabled)
    out.writeInt(address)
    out.writeBoolean(addressAutoIncrement)
    out.writeInt(enabledChannels)
    out.writeInt(cycles)
    out.writeObject(channelOutput)
    out.writeInt(channelIndex)
    out.writeDouble(sample)

  def loadState(in:ObjectInputStream): Unit =
    NESComponent.loadMemory(ram,in)
    soundEnabled = in.readBoolean()
    address = in.readInt()
    addressAutoIncrement = in.readBoolean()
    enabledChannels = in.readInt()
    cycles = in.readInt()
    NESComponent.loadMemory(channelOutput,in)
    channelIndex = in.readInt()
    sample = in.readDouble()

  def enableSound(enabled:Boolean): Unit =
    soundEnabled = enabled

  def setAddressPort(value:Int): Unit =
    address = value & 0x7F
    addressAutoIncrement = (value & 0x80) > 0

  def readDataPort(): Int =
    val r = ram(address)
    if addressAutoIncrement then
      address = (address + 1) & 0x7F

    r

  def writeDataPort(value:Int): Unit =
    ram(address) = value
    if address == 0x7F then
      enabledChannels = (value >> 4) & 7

    if addressAutoIncrement then
      address = (address + 1) & 0x7F

  def getSample: Double = if soundEnabled then sample else 0.0

  private inline def sample(x:Int): Int = (ram(x >> 1) >> ((x & 1) << 2)) & 0x0F

  private inline def clockChannel(): Unit =
    val channel = 7 - channelIndex
    val base = 0x40 + (channel << 3)
    var phase = (ram(base + 5) << 16) | (ram(base + 3) << 8) | ram(base + 1)
    val freq = ((ram(base + 4) & 0x3) << 16) | (ram(base + 2) << 8) | ram(base)
    val length = 256 - (ram(base + 4) & 0xFC)
    val offset = ram(base + 6)
    val volume = ram(base + 7) & 0x0F

    phase = (phase + freq) % (length << 16)
    ram(base + 5) = (phase >> 16) & 0xFF
    ram(base + 3) = (phase >> 8) & 0xFF
    ram(base + 1) = phase & 0xFF

    channelOutput(channel) = (sample(((phase >> 16) + offset) & 0xFF) - 8) * volume // [-120,105]

    sample = 0.0
    var c = 0
    while c <= enabledChannels do
      sample += channelOutput(7 - c) + 120 // [0,225]
      c += 1

    // normalization
    sample = sample / (225.0 * (enabledChannels + 1)) // [0,1]

  def clock(): Unit =
    cycles += 1
    if cycles == 15 then
      cycles = 0
      clockChannel()
      if channelIndex < enabledChannels then
        channelIndex += 1
      else channelIndex = 0