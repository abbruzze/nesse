package ucesoft.nes.util

import java.awt.Image
import java.io.{BufferedOutputStream, ByteArrayInputStream, ByteArrayOutputStream, File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import ucesoft.nes.{Display, Log, NES, NESComponent}

import java.util.concurrent.CountDownLatch
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import javax.imageio.ImageIO
import scala.collection.mutable.ListBuffer

object StateSnapShotManager:
  case class Snap(name:String,ts:Long,image:Image)

class StateSnapShotManager(nes:NES,display:Display) extends Runnable:
  import StateSnapShotManager.Snap

  private[this] var thread : Thread = _
  private[this] var snapPeriodInMillis = 1000
  private[this] val snapDir : File = {
    var nesseHome = System.getProperty("nesse.home")
    if nesseHome == null then nesseHome = scala.util.Properties.userHome
    val dir = new File(nesseHome,"snapshots")
    if !dir.exists() then dir.mkdirs()
    dir
  }
  private[this] var snapIndex = 0
  private[this] var size = 60
  @volatile private[this] var running = false
  private[this] var countDownShutdown = new CountDownLatch(1)
  private[this] val buffer = new ByteArrayOutputStream(128 * 1024)

  // CONSTRUCTOR ============================
  init()
  //=========================================

  private def init(): Unit =
    clear()

  def lastSnapList(): List[String] =
    val buffer = new ListBuffer[String]
    val firstIndex = snapIndex
    var ptr = if firstIndex == 0 then size - 1 else firstIndex - 1
    while ptr != firstIndex do
      val file = new File(snapDir,s"snap_$ptr")
      if file.exists() then buffer += file.getName
      ptr = if ptr == 0 then size - 1 else ptr - 1

    buffer.toList

  def getSnapStream(name:String) : Option[ObjectInputStream] =
    val snap = new File(snapDir,name)
    if !snap.exists() then return None

    try
      val in = new ObjectInputStream(new GZIPInputStream(new FileInputStream(snap)))
      val ts = in.readLong() // skip ts
      //val image = ImageIO.read(in) // skip image
      in.readObject()
      Some(in)
    catch
      case t:Throwable =>
        println(s"Can't read snap $snap")
        t.printStackTrace()
        None

  def getSnap(name:String): Option[Snap] =
    val snap = new File(snapDir,name)
    if !snap.exists() then return None

    try
      val in = new ObjectInputStream(new GZIPInputStream(new FileInputStream(snap)))
      val ts = in.readLong()
      val imageBuffer = new ByteArrayInputStream(in.readObject().asInstanceOf[Array[Byte]])
      val image = ImageIO.read(imageBuffer)
      in.close()
      Some(Snap(name,ts,image))
    catch
      case t:Throwable =>
        println(s"Can't read snap $snap")
        None

  def clear(): Unit =
    for f <- snapDir.listFiles() do
      f.delete()

    snapIndex = 0

  def start(): Unit =
    if thread == null then
      thread = new Thread(this,"SnapShot")
      thread.start()

  def isRunning: Boolean = running

  def stop(): Unit =
    if running then
      running = false
      countDownShutdown.await()
      countDownShutdown = new CountDownLatch(1)

  def setPeriod(periodInSeconds:Int,size:Int): Unit =
    snapPeriodInMillis = periodInSeconds * 1000
    this.size = size

  override def run(): Unit =
    Log.info("StateSnapshot Thread started")
    running = true
    var elapsed = 0
    while running do
      Thread.sleep(snapPeriodInMillis - elapsed)
      val ts = System.currentTimeMillis()
      nes.clk.pause
      val state = makeSnap()
      nes.clk.play
      saveSnap(state)
      elapsed = (System.currentTimeMillis() - ts).toInt
      if elapsed > snapPeriodInMillis then elapsed = snapPeriodInMillis

    thread = null
    countDownShutdown.countDown()
    Log.info("StateSnapshot Thread stopped")

  private def makeSnap() : Array[Byte] =
    val out = new ObjectOutputStream(buffer)
    out.writeLong(System.currentTimeMillis())
    val imageBuffer = new ByteArrayOutputStream()
    ImageIO.write(ImageScaler.resizeImage(display.getSnapshot(),32*8*2,30*8*2),"png",imageBuffer)
    out.writeObject(imageBuffer.toByteArray)
    try
      nes.setSaveSnapShotMode(true)
      nes.save(out)
      out.flush()
    finally
      nes.setSaveSnapShotMode(false)

    val array = buffer.toByteArray
    buffer.reset()
    array

  private def saveSnap(state:Array[Byte]): Unit =
    val fout = new FileOutputStream(new File(snapDir,s"snap_$snapIndex"))
    val out = new GZIPOutputStream(new BufferedOutputStream(fout))
    out.write(state)
    out.close()
    snapIndex = (snapIndex + 1) % size
