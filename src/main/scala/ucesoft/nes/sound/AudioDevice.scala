package ucesoft.nes.sound

import ucesoft.nes.{Clock, NESComponent, NESComponentType}

import java.util.Properties
import java.util.concurrent.LinkedBlockingDeque
import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, SourceDataLine}

class AudioDevice(sampleRate:Int) extends NESComponent with Runnable:
  override val componentID: String = "AudioDevice"
  override val componentType: NESComponentType = NESComponentType.AUDIO
  
  private var CPU_CYCLE_PER_SECOND = 1789772
  private var cyclePerSample = CPU_CYCLE_PER_SECOND / sampleRate
  private val queue = new LinkedBlockingDeque[Array[Byte]]
  private var bufferSize: Int = 0

  private var buffer: Array[Byte] = _
  private var cycle = 0L

  private var bufferId: Int = 0
  private var bufferPos: Int = 0
  private var bufferInMillis = 10
  private val thread = new Thread(this,"AudioDevice")
  private var muted = false
  private val muteLock = new Object
  private var sourceLine : SourceDataLine = _
  private var stopped = false

  setBufferInMillis(bufferInMillis)
  thread.setPriority(Thread.MAX_PRIORITY)

  override def getProperties: Properties = 
    val p = super.getProperties
    p.setProperty("Samples queue size:",queue.size().toString)
    p.setProperty("Buffer in milliseconds:",bufferInMillis.toString)
    p
  
  def setCPUFrequency(f:Float): Unit =
    CPU_CYCLE_PER_SECOND = f.toInt
    cyclePerSample = CPU_CYCLE_PER_SECOND / sampleRate
    reset

  def setBufferInMillis(bim:Int) : Unit =
    bufferInMillis = bim
    bufferSize = 2 * (sampleRate * bim / 1000.0).toInt
    buffer = Array.ofDim[Byte](bufferSize)

  /**
   * Value must be in [0..1]
   * @param value
   */
  final def addSample(value: Double): Unit =
    if !muted then
      if cycle == cyclePerSample then
        // normalize
        val level = (value * 0xFFFF).toInt - 32767
        cycle = 0
        buffer(bufferId) = (level >> 8).asInstanceOf[Byte]
        buffer(bufferId + 1) = (level & 0xFF).asInstanceOf[Byte]
        bufferId += 2
        if bufferId == bufferSize then
          queue.put(buffer)
          buffer = Array.ofDim[Byte](bufferSize)
          bufferId = 0

      cycle += 1
  
  override def init: Unit = {}
  
  override def reset: Unit =
    queue.clear()

  inline private def dequeue() : Array[Byte] =
    muteLock.synchronized {
      while muted do
        muteLock.wait()
    }
    queue.take()


  def start(): Unit =
    if !thread.isAlive then thread.start()

  def isMuted() : Boolean = muted

  def mute(muted:Boolean) : Unit =
    muteLock.synchronized {
      this.muted = muted
      muteLock.notify()
    }

  def stop(): Unit =
    stopped = true

  override def run(): Unit =
    try {
      val format = new AudioFormat(sampleRate.toFloat, 16, 1, true, true)

      val info = new DataLine.Info(classOf[SourceDataLine], format)
      sourceLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
      sourceLine.open(format)

      sourceLine.start()
    }
    catch {
      case t:Throwable =>
        println(s"Cannot start audio system: $t")
        return
    }

    while !stopped do
      val samples = queue.take()
      val available = sourceLine.available()
      if available >= samples.length then
        sourceLine.write(samples,0,samples.length)
      else
        sourceLine.write(samples,0,available)

    sourceLine.drain()
    sourceLine.close()