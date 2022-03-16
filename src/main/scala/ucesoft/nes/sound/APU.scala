package ucesoft.nes.sound

import ucesoft.nes.{Cartridge, ChipID, NESComponent, NESComponentType}
import ucesoft.nes.cpu.Memory

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

object APU:
  /**
   * The DACs for each channel are implemented in a way that causes non-linearity
    and interaction between channels, so calculation of the resulting amplitude is
    somewhat involved.

    The normalized audio output level is the sum of two groups of channels:

        output = square_out + tnd_out


                              95.88
        square_out = -----------------------
                            8128
                     ----------------- + 100
                     square1 + square2


                              159.79
        tnd_out = ------------------------------
                              1
                  ------------------------ + 100
                  triangle   noise    dmc
                  -------- + ----- + -----
                    8227     12241   22638


    where triangle, noise, dmc, square1 and square2 are the values fed to their
    DACs. The dmc ranges from 0 to 127 and the others range from 0 to 15. When the
    sub-denominator of a group is zero, its output is 0. The output ranges from 0.0
    to 1.0.
   */
  private final val SQUARE_TABLE =
    (0 until 31) map { n => 95.52 / (8128.0 / n + 100) } toArray
  private final val TND_TABLE =
    (0 until 203) map { n => 163.67 / (24329.0 / n + 100) } toArray

class APU(irqLow:Boolean => Unit,dmaReadSampleHandler : Int => Int,audioDevice:AudioDevice) extends NESComponent with Memory with Clocked:
  import APU.*

  override val name: String = "APUMemory"
  override val componentID: String = "APU"
  override val componentType: NESComponentType = NESComponentType.APU

  private[this] val pulse1 = new Pulse(1)
  private[this] val pulse2 = new Pulse(2)
  private[this] val triangle = new Triangle
  private[this] val noise = new Noise
  private[this] val dmc = new DMC(irqLow => {
    dmcIRQ = irqLow
    checkIRQ()
  },dmaReadSampleHandler)

  private[this] val channels = Array(pulse1, pulse2, triangle, noise, dmc)

  private[this] var frameCounterIRQ, dmcIRQ = false
  private[this] var frameCounterTimer = -1
  private[this] var timer = 0
  private[this] var step = 1
  private[this] var cart : Cartridge = _
  private[this] var cartGenerateSound = false
  private[this] var cartAudioRatio = 0.0

  private[this] final val FRAME_COUNTER_PERIOD = 7458

  private val regs = Array.ofDim[Int](0x20)

  override def getProperties: Properties =
    val p = super.getProperties
    p.setProperty("Pulse 1 output",pulse1.output().toString)
    p.setProperty("Pulse 2 output",pulse2.output().toString)
    p.setProperty("Triangle output",triangle.output().toString)
    p.setProperty("Noise output",noise.output().toString)
    p.setProperty("DMC output",dmc.output().toString)
    p.setProperty("Cartridge output",if cart != null then cart.getAudioSample().toString else "0")
    p.setProperty("Frame Counter IRQ",frameCounterIRQ.toString)
    p.setProperty("DMC IRQ",dmcIRQ.toString)
    p.setProperty("Frame Counter timer",frameCounterTimer.toString)
    p.setProperty("Timer",timer.toString)
    var r = 0
    while r < 0x20 do
      p.setProperty(s"Reg $r",s"0x${regs(r).toHexString}")
      r += 1
    p

  private def checkIRQ() : Unit =
    irqLow((!isInterruptDisabled() && frameCounterIRQ) || dmcIRQ)

  def setCartridge(cart:Cartridge): Unit =
    this.cart = cart
    setTV(cart.ines.tv)
    cartGenerateSound = cart.hasAudio
    cartAudioRatio = cart.getAudioRatio()

  def setTV(tv:Cartridge.TV): Unit =
    for c <- channels do c.setTV(tv)

  def setSample(sample:Int): Unit =
    dmc.setSample(sample)

  override def reset: Unit = {
    for(c <- channels) c.reset()
    frameCounterIRQ = false
    dmcIRQ = false
    frameCounterTimer = -1
    timer = 0
    step = 1
    java.util.Arrays.fill(regs,0)
    regs(0x17) = 0x40 // IRQ disable flag
    //write(0x4017,0)
  }

  override def read(address: Int, chipID: ChipID): Int =
    address match
      case 0x4015 =>
        var active = 0
        var i = 0
        while i < channels.length do
          if channels(i).isActive() then active |= 1 << i
          active |= (if channels(i).getInterruptFlag() then 0x80 else 0)
          i += 1

        active |= (if frameCounterIRQ then 0x40 else 0)

        clearStatusFrameCounterInterrupt()
        active

  override def write(address: Int, value: Int, chipID: ChipID): Unit =
    regs(address & 0x1F) = value

    address match
      case 0x4015 =>
        var sel = value
        var s = 0
        while s < channels.length do
          channels(s).setEnabled((sel & 1) != 0)
          sel >>= 1
          s += 1
        dmc.clearInterruptFlag()
      case 0x4017 =>
        frameCounterTimer = -3
        timer = 0
        step = 1
        if (value & 0x80) > 0 then
          clockSubComponents(true,true)

        if isInterruptDisabled() then frameCounterIRQ = false
        checkIRQ()
      case adr =>
        val a = (adr & 0x1F) >> 2
        if a < channels.length then
          channels(a).setRegister(adr & 3,value)

  inline private def clockSubComponents(envelopeAndLinearCounter:Boolean,lengthCounterAndSweep:Boolean) : Unit =
    var c = 0
    while c < channels.length do
      if envelopeAndLinearCounter then channels(c).clockEnvelopAndLinearCounter()
      if lengthCounterAndSweep then channels(c).clockLengthCounterAndSweep()
      c += 1

  final override def clock() : Unit =
    if timer == FRAME_COUNTER_PERIOD then
      timer = 0
      val mode = getStepMode()
      mode match {
        case 4 =>
          step match
            case 1 | 3 =>
              clockSubComponents(true,false)
            case m@(2 | 4) =>
              clockSubComponents(true,true)
              if m == 4 && !isInterruptDisabled() then setStatusFrameCounterInterrupt()
          if step < 4 then step += 1 else step = 1
        case 5 =>
          step match
            case 1 | 3 =>
              clockSubComponents(true,true)
            case 2 | 4 =>
              //for (generator <- channels) generator.clockEnvelopAndLinearCounter()
              clockSubComponents(true,false)
            case 5 =>
          if step < 5 then step += 1 else step = 1
      }
    else
      timer += 1

    var c = 0
    while c < channels.length do
      channels(c).clock()
      c += 1

    val square_out = SQUARE_TABLE(pulse1.output() + pulse2.output())
    val tnd_out = TND_TABLE(3 * triangle. output() + 2 * noise.output() + dmc.output())
    var audioSample = square_out + tnd_out
    if cartGenerateSound then
      audioSample = audioSample * cartAudioRatio + (1.0 - cartAudioRatio) * cart.getAudioSample()
    audioDevice.addSample(audioSample)

  inline private def isInterruptDisabled() = (regs(0x17) & 0x40) != 0

  inline private def getStepMode() = if (regs(0x17) & 0x80) != 0 then 5 else 4

  inline private def clearStatusFrameCounterInterrupt(): Unit =
    regs(0x15) &= ~0x40
    frameCounterIRQ = false
    checkIRQ()

  inline private def setStatusFrameCounterInterrupt(): Unit =
    regs(0x15) |= 0x40
    frameCounterIRQ = true
    checkIRQ()

  override def saveState(out: ObjectOutputStream): Unit =
    var c = 0
    while c < channels.length do
      channels(c).saveState(out)
      c += 1

    out.writeObject(regs)
    out.writeBoolean(frameCounterIRQ)
    out.writeBoolean(dmcIRQ)
    out.writeInt(frameCounterTimer)
    out.writeInt(timer)
    out.writeInt(step)

  override def loadState(in: ObjectInputStream): Unit =
    var c = 0
    while c < channels.length do
      channels(c).loadState(in)
      c += 1

    loadMemory(regs,in)
    frameCounterIRQ = in.readBoolean()
    dmcIRQ = in.readBoolean()
    frameCounterTimer = in.readInt()
    timer = in.readInt()
    step = in.readInt()