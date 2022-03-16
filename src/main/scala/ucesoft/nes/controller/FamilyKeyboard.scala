package ucesoft.nes.controller
import ucesoft.nes.ClockEvent
import ucesoft.nes.controller.FamilyKeyboard.KeyboardLayout

import java.awt.event.{KeyAdapter, KeyEvent}
import java.io.*
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.annotation.tailrec

object FamilyKeyboard:
  enum TapeState:
    case STOP,PLAY,RECORD

  enum Key:
    case F1,F2,F3,F4,F5,F6,F7,F8
    case _0,_1,_2,_3,_4,_5,_6,_7,_8,_9,MINUS,POWER,UNKNOWN,STOP,CLR,INS,DEL
    case ESC,Q,W,E,R,T,Y,U,I,O,P,AT,OPEN_RECT,RETURN,UP
    case CTR,A,S,D,F,G,H,J,K,L,SEMICOLON,COLON,CLOSED_RECT,KANA,LEFT,RIGHT
    case LSHIFT,Z,X,C,V,B,N,M,COMMA,PERIOD,SLASH,UNDERSCORE,RSHIFT,DOWN
    case GRPH,SPACE

  import Key.*

  import java.awt.event.KeyEvent.*

  val MATRIX : Array[Array[Array[Key]]] = Array(
    // Column 0
    Array(
      // Row 0
      Array(F8,RETURN,OPEN_RECT,CLOSED_RECT),
      // Row 1
      Array(F7,AT,COLON,SEMICOLON),
      // Row 2
      Array(F6,O,L,K),
      // Row 3
      Array(F5,I,U,J),
      // Row 4
      Array(F4,Y,G,H),
      // Row 5
      Array(F3,T,R,D),
      // Row 6
      Array(F2,W,S,A),
      // Row 7
      Array(F1,ESC,Q,CTR),
      // Row 8
      Array(CLR,UP,RIGHT,LEFT)
    ),
    // Column 1
    Array(
      // Row 0
      Array(KANA,RSHIFT,UNKNOWN,STOP),
      // Row 1
      Array(UNDERSCORE,SLASH,MINUS,POWER),
      // Row 2
      Array(PERIOD,COMMA,P,_0),
      // Row 3
      Array(M,N,_9,_8),
      // Row 4
      Array(B,V,_7,_6),
      // Row 5
      Array(F,C,_5,_4),
      // Row 6
      Array(X,Z,E,_3),
      // Row 7
      Array(LSHIFT,GRPH,_1,_2),
      // Row 8
      Array(DOWN,SPACE,DEL,INS)
    )
  )

  private val KEY_EVENT_MAP = getKeyEventMap
  private val KEY_EVENT_REV_MAP = getKeyEventMap map { kv => (kv._2,kv._1) }

  private def getKeyEventMap : Map[Int,String] =
    val clazz = classOf[KeyEvent]
    val fields = clazz.getDeclaredFields
    fields filter { _.getName.startsWith("VK_") } map { f => (f.get(null).asInstanceOf[Int],f.getName) } toMap


  type KeyboardLayout = Map[Int,Key]

  val DEFAULT_LAYOUT : KeyboardLayout = Map(
    VK_F1 -> F1,
    VK_F2 -> F2,
    VK_F3 -> F3,
    VK_F4 -> F4,
    VK_F5 -> F5,
    VK_F6 -> F6,
    VK_F7 -> F7,
    VK_F8 -> F8,
    VK_1 -> _1,
    VK_2 -> _2,
    VK_3 -> _3,
    VK_4 -> _4,
    VK_5 -> _5,
    VK_6 -> _6,
    VK_7 -> _7,
    VK_8 -> _8,
    VK_9 -> _9,
    VK_0 -> _0,
    VK_MINUS -> MINUS,
    16777452 -> POWER, // ì
    VK_PAGE_UP -> UNKNOWN,
    VK_END -> STOP,
    VK_HOME -> CLR,
    VK_INSERT -> INS,
    VK_BACK_SPACE -> DEL,
    VK_ESCAPE -> ESC,
    VK_Q -> Q,
    VK_W -> W,
    VK_E -> E,
    VK_R -> R,
    VK_T -> T,
    VK_Y -> Y,
    VK_U -> U,
    VK_I -> I,
    VK_O -> O,
    VK_P -> P,
    VK_AT -> AT,
    16777448 -> OPEN_RECT, // è
    VK_ENTER -> RETURN,
    VK_UP -> UP,
    VK_CONTROL -> CTR,
    VK_A -> A,
    VK_S -> S,
    VK_D -> D,
    VK_F -> F,
    VK_G -> G,
    VK_H -> H,
    VK_J -> J,
    VK_K -> K,
    VK_L -> L,
    16777458 -> SEMICOLON, // ò
    16777440 -> COLON, // à
    VK_PLUS -> CLOSED_RECT,
    16777465 -> KANA, // ù
    VK_LEFT -> LEFT,
    VK_RIGHT -> RIGHT,
    VK_Z -> Z,
    VK_X -> X,
    VK_C -> C,
    VK_V -> V,
    VK_B -> B,
    VK_N -> N,
    VK_M -> M,
    VK_COMMA -> COMMA,
    VK_PERIOD -> PERIOD,
    VK_QUOTE -> SLASH,
    VK_GREATER -> UNDERSCORE,
    VK_DOWN -> DOWN,
    VK_F9 -> GRPH,
    VK_SPACE -> SPACE
  )

  def saveLayout(file:String,layout:KeyboardLayout): Unit =
    val out = new PrintWriter(new FileOutputStream(file),true)
    try
      for kv <- layout do
        KEY_EVENT_MAP get kv._1 match
          case Some(k) =>
            out.println("%20s = %s".format(s"$k",s"${kv._2}"))
          case None =>
            out.println("%20s = %s".format(s"#${kv._1}",s"${kv._2}"))
    finally
      out.close()

  def loadLayout(file:String): Option[KeyboardLayout] =
    val src = io.Source.fromFile(file)
    try
      val lines = src.getLines().toList
      val map = for line <- lines yield
        line.split("=") match
          case Array(n,v) =>
            val k = n.trim
            val key = if (k.charAt(0) == '#') k.substring(1).toInt else KEY_EVENT_REV_MAP(k)
            key -> Key.valueOf(v.trim)
          case _ =>
            return None
      Some(map.toMap)
    catch
      case _:Exception =>
        None
    finally
      src.close()




class FamilyKeyboard(var layout : KeyboardLayout) extends InputDevice:
  override val name = "FamilyKeyboard"
  import FamilyKeyboard.*
  override val inputType: InputType = InputType.FamilyKeyboard

  private class BitBuffer:
    private final val BLOCK = 1024
    private var data = Array.ofDim[Int](BLOCK)
    private var size = 0
    private var capacity = BLOCK << 5

    inline private def checkSize(): Unit =
      if capacity < size + 1 then
        val newData = Array.ofDim[Int](data.length + BLOCK)
        System.arraycopy(data,0,newData,0,data.length)
        data = newData
        capacity = data.length << 5

    def addBit(bit:Int): Unit =
      checkSize()
      data(size >> 5) |= (bit & 1) << (size & 0x1F)
      size += 1

    def getBit(index:Int): Int =
      if index < size then
        (data(index >> 5) >> (index & 0x1F)) & 1
      else 0

    def clear(): Unit =
      size = 0
      capacity = BLOCK << 5
      data = Array.ofDim[Int](BLOCK)

    def save(out:ObjectOutputStream): Unit =
      out.writeInt(capacity)
      out.writeInt(size)
      out.writeObject(data)

    def load(in:ObjectInputStream): Unit =
      capacity = in.readInt()
      size = in.readInt()
      data = in.readObject().asInstanceOf[Array[Int]]

  private[this] final val DEFAULT_SAMPLE_CYCLES = 88 * 3 // 3x because the clock cycles are PPU clock cycles
  private[this] var row = 0
  private[this] var col = 0
  private[this] var enableBit = false
  private[this] val keyPressed = collection.mutable.Set.empty[Key]
  private[this] var tapeState : TapeState = TapeState.STOP
  private[this] var tapeEnabled = false
  private[this] var tapeIndex = 0
  private[this] val buffer = new BitBuffer
  private[this] var tapeBit = 0
  private[this] val clk = ucesoft.nes.Clock.systemClock
  private[this] var sampleCycles = DEFAULT_SAMPLE_CYCLES

  def save(file:String): Unit =
    val out = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(file)))
    buffer.save(out)
    out.close()

  def load(file:String): Unit =
    val in = new ObjectInputStream(new GZIPInputStream(new FileInputStream(file)))
    buffer.load(in)
    in.close()

  def getSampleCycles(): Int = sampleCycles
  def setSampleCycles(sc:Int): Unit =
    sampleCycles = sc

  def setTapeState(state:TapeState): Unit =
    tapeState = state
    state match
      case TapeState.RECORD =>
        buffer.clear()
        clk.cancel("TapeClock")
        clk.schedule(new ClockEvent("TapeClock",clk.currentCycles + sampleCycles,clockRecord))
      case TapeState.PLAY =>
        clk.cancel("TapeClock")
        clk.schedule(new ClockEvent("TapeClock",clk.currentCycles + sampleCycles,clockPlay))
      case TapeState.STOP =>
        clk.cancel("TapeClock")

    tapeIndex = 0

  override def eject(): Unit =
    clk.cancel("TapeClock")

  private def clockRecord(cycles:Long): Unit =
    buffer.addBit(tapeBit)
    clk.schedule(new ClockEvent("TapeClock",cycles + sampleCycles,clockRecord))

  private def clockPlay(cycles:Long): Unit =
    tapeBit = buffer.getBit(tapeIndex)
    tapeIndex += 1
    clk.schedule(new ClockEvent("TapeClock",cycles + sampleCycles,clockPlay))

  override def readPort(port: Int): Int =
    if port == 1 then
      tapeState match
        case TapeState.PLAY =>
          return tapeBit << 1
        case _ =>
          return 0
    
    var read = 0x1E
    if enabled then
      val keys = MATRIX(col)(row)
      var b = 1
      while b < 5 do
        if keyPressed.contains(keys(b - 1)) then read &= ~(1 << b)
        b += 1

    read

  override def writePort(port: Int, value: Int): Unit =
    if port == 1 then
      tapeEnabled = (value & 4) > 0
      tapeState match
        case TapeState.RECORD if tapeEnabled =>
          tapeBit = value & 1
        case _ =>
      val reset = (value & 1) == 1
      if reset then row = 0
      val oldCol = col
      col = (value >> 1) & 1
      if !reset && oldCol == 1 && col == 0 then
        if row == 8 then row = 1 else row += 1
      enableBit = (value & 4) > 0

  override def keyTyped(e: KeyEvent): Unit = {}

  override def keyPressed(e: KeyEvent): Unit =
    if (e.isAltDown || !enabled) return
    val key = if (e.getKeyCode != KeyEvent.VK_UNDEFINED) e.getKeyCode else e.getExtendedKeyCode

    if key == KeyEvent.VK_SHIFT then
      val shift = if e.getKeyLocation == KeyEvent.KEY_LOCATION_LEFT then Key.LSHIFT else Key.RSHIFT
      keyPressed += shift
    else
      layout.get(key) match
        case Some(k) =>
          keyPressed += k
        case _ =>

  override def keyReleased(e: KeyEvent): Unit =
    if (e.isAltDown || !enabled) return
    val key = if (e.getKeyCode != KeyEvent.VK_UNDEFINED) e.getKeyCode else e.getExtendedKeyCode

    if key == KeyEvent.VK_SHIFT then
      val shift = if e.getKeyLocation == KeyEvent.KEY_LOCATION_LEFT then Key.LSHIFT else Key.RSHIFT
      keyPressed -= shift
    else
      layout.get(key) match
      case Some(k) =>
        keyPressed -= k
      case _ =>

  override def reset: Unit =
    keyPressed.clear()
    enableBit = false
    row = 0
    col = 0
