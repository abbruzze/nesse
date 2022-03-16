package ucesoft.nes.controller.rob

import ucesoft.nes.controller.Joystick
import ucesoft.nes.cpu.Memory
import ucesoft.nes.{ChipID, Clock, ClockEvent}

import java.awt.{BasicStroke, Color, Graphics, Graphics2D}
import java.awt.geom.{AffineTransform, Ellipse2D, GeneralPath, Line2D, RoundRectangle2D}
import java.io.{ObjectInputStream, ObjectOutputStream}
import javax.imageio.ImageIO
import javax.swing.{JComponent, JFrame, WindowConstants}

class ROBStackUpController(override val mem:Memory,override val controllerA:Joystick,override val controllerB:Joystick) extends ROBController(mem,controllerA,controllerB):
  override protected type MatrixCell = Position
  protected enum Position(val color:Option[Color]):
    case Empty extends Position(None)
    case Red extends Position(Some(Color.RED))
    case White extends Position(Some(Color.WHITE.darker()))
    case Blue extends Position(Some(Color.BLUE))
    case Yellow extends Position(Some(Color.YELLOW))
    case Green extends Position(Some(Color.GREEN))

  private[this] final val MODE_ADDRESS = 0x0038
  private[this] final val TARGET_PLACEMENT_ADDRESS = 0x500
  override protected val matrix = Array.ofDim[Position](5,6)
  private[this] val targetMatrix = Array.ofDim[Position](5,6)
  private[this] var frameCounter = 0
  private[this] var lastMode = 0
  private[this] val clk = Clock.systemClock
  private[this] val renderer = new ROBStackUpRenderer
  private[this] val rendererFrame : JFrame = {
    val f = new JFrame("R.O.B.")
    f.getContentPane.add("Center",renderer)
    f.setSize(ROBStackUpRenderer.WIDTH,ROBStackUpRenderer.HEIGHT)
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    f.setIconImage(ImageIO.read(getClass.getResourceAsStream("/resources/images/nesse_logo.png")))
    f
  }

  private object ROBStackUpRenderer:
    final val WIDTH = 800
    final val HEIGHT = 400

  private class ROBStackUpRenderer extends JComponent {
    import ROBStackUpRenderer.*

    private[this] val BASE_POLY = Array(
      (4, 0), (4, -1), (2, -1), (2, -3), (4, -3), (4, -7),
      (-4, -7), (-4, -3), (-2, -3), (-2, -1), (-4, -1), (-4, 0)
    )
    private[this] val BASE = createPoly(BASE_POLY)
    private[this] val STACK_POLY = Array(
      (-6,2),(6,2),(6,-2),(2,-2),(1,-3),
      (-1,-3),(-2,-2),(-6,-2),(-6,2)
    )
    private[this] val STACK = createPoly(STACK_POLY)
    private[this] val ARM_OPEN_RIGHT_POLY = Array(
      (7,2),(8,2),(8,1),(12,1),
      (12,-1),(8,-1),(8,-2),(7,-2)
    )
    private[this] val ARM_OPEN_RIGHT = createPoly(ARM_OPEN_RIGHT_POLY)
    private[this] val ARM_OPEN_LEFT_POLY = ARM_OPEN_RIGHT_POLY.map(c => (-c._1,c._2))
    private[this] val ARM_OPEN_LEFT = createPoly(ARM_OPEN_LEFT_POLY)

    private def createPoly(points: Array[(Int, Int)]): GeneralPath =
      val path = new GeneralPath()
      var moveOnNext = true
      for p <- points do
        if p._1 == Int.MaxValue then moveOnNext = true
        else
          if moveOnNext then
            moveOnNext = false
            path.moveTo(points(0)._1.toFloat, -points(0)._2.toFloat)
          else
            path.lineTo(p._1.toFloat, -p._2.toFloat)
      path.closePath()
      path

    override def paint(g: Graphics): Unit =
      val g2 = g.asInstanceOf[Graphics2D]
      val dim = getSize()
      val at = new AffineTransform()
      val scaleT = new AffineTransform()
      val xscaleFactor = dim.width.toDouble / WIDTH * 6
      val yscaleFactor = dim.height.toDouble / HEIGHT * 6
      val ybase = dim.height / 2
      val yHeight = (STACK.getBounds.height - 1) * yscaleFactor
      scaleT.scale(xscaleFactor, yscaleFactor)
      val trackSize = dim.width / 6
      g2.setStroke(new BasicStroke(0.6))

      g2.setColor(Color.WHITE)
      g2.fillRect(0, 0, dim.width, dim.height)

      for x <- 0 to 4 do
        for y <- 0 to 5 do
          matrix(x)(y).color match
            case Some(color) =>
              at.setToIdentity()
              at.translate((x + 1) * trackSize, ybase + y * yHeight)
              at.concatenate(scaleT)
              g2.setTransform(at)
              g2.setColor(Color.BLACK)
              g2.draw(STACK)
              g2.setColor(color)
              g2.fill(STACK)
            case None =>

      for x <- 0 to 4 do
        at.setToIdentity()
        at.translate((x + 1) * trackSize, ybase + 5.5 * yHeight)
        at.concatenate(scaleT)
        g2.setTransform(at)
        g2.setColor(Color.BLACK)
        g2.draw(BASE)
        g2.setColor(Color.ORANGE)
        g2.fill(BASE)

      val offset = if arm.state == ArmState.Open then 0 else 1
      at.setToIdentity()
      at.translate((arm.x + 1) * trackSize - xscaleFactor * offset, ybase + arm.y * yHeight)
      at.concatenate(scaleT)
      g2.setTransform(at)
      g2.setColor(Color.BLACK)
      g2.draw(ARM_OPEN_RIGHT)
      g2.setColor(Color.DARK_GRAY)
      g2.fill(ARM_OPEN_RIGHT)
      at.setToIdentity()
      at.translate((arm.x + 1) * trackSize + xscaleFactor * offset, ybase + arm.y * yHeight)
      at.concatenate(scaleT)
      g2.setTransform(at)
      g2.setColor(Color.BLACK)
      g2.draw(ARM_OPEN_LEFT)
      g2.setColor(Color.DARK_GRAY)
      g2.fill(ARM_OPEN_LEFT)

      // HEAD
      at.setToIdentity()
      at.translate(dim.width / 2,dim.height / 6)
      at.concatenate(scaleT)
      g2.setTransform(at)
      g2.setColor(Color.BLACK)
      g2.draw(new RoundRectangle2D.Float(-6,-2,12,4,2,2))
      val eye1 = new Ellipse2D.Float(-5,-1,2,2)
      val eye2 = new Ellipse2D.Float(3,-1,2,2)
      g2.draw(eye1)
      g2.draw(eye2)
      g2.draw(new Line2D.Float(-2,-1,2,-1))
      g2.draw(new Line2D.Float(-2,0,2,0))
      g2.draw(new Line2D.Float(-2,1,2,1))
      if eyesOpen then g2.setColor(Color.RED) else g2.setColor(Color.CYAN)
      g2.fill(eye1)
      g2.fill(eye2)

      at.setToIdentity()
      g2.setTransform(at)
  }

  reset()

  override def getRenderer: JFrame = rendererFrame
  def updateRenderer(): Unit =
    renderer.repaint()

  override def onFrame(): Unit =
    frameCounter += 1
    if frameCounter == 60 then
      frameCounter = 0
      checkMem()

  private def checkMem(): Unit =
    val mode = mem.read(MODE_ADDRESS)
    if mode != lastMode then
      lastMode = mode
      reset()
    var ptr = TARGET_PLACEMENT_ADDRESS
    for y <- 0 to 5 ; x <- 0 to 4 do
      val color = mem.read(ptr)
      ptr += 1
      if color < 6 then
        targetMatrix(x)(y) = Position.fromOrdinal(color)

  override def reset(): Unit =
    import Position.*
    super.reset()

    for c <- 0 until 5 ; r <- 0 until 6 do
      matrix(c)(r) = Empty
    
    if lastMode == 4 then
      matrix(2)(3) = Red
      matrix(2)(4) = White
      matrix(2)(5) = Blue
      matrix(1)(5) = Yellow
      matrix(3)(5) = Green
    else
      matrix(2)(1) = Red
      matrix(2)(2) = White
      matrix(2)(3) = Blue
      matrix(2)(4) = Yellow
      matrix(2)(5) = Green
    frameCounter = 0

    updateRenderer()

  override def getCommand(cmd: Int): Option[Command] =
    import Command.*
    cmd match
      case 0xFA => Some(UP)
      case 0xAE => Some(DOWN)
      case 0xBA => Some(LEFT)
      case 0xEA => Some(RIGHT)
      case 0xBE => Some(CLOSE)
      case 0xEE => Some(OPEN)
      case 0xEB => Some(TEST)
      case _ => None

  override def executeCommand(cmd: Command): Unit =
    import ArmState.*
    import Command.*
    import Position.*
    cmd match
      case TEST =>
        eyesOpen = true
      case UP =>
        if arm.y > 0 then
          if arm.state == Close then
            if cellAt(arm.x,0) == Empty then
              for y <- 0 to arm.y - 1 do
                matrix(arm.x)(y) = matrix(arm.x)(y + 1)
              matrix(arm.x)(arm.y) = Position.Empty
              arm = arm.copy(y = arm.y - 1)
          else
            arm = arm.copy(y = arm.y - 1)
      case DOWN =>
        if arm.y < 5 && (arm.state == Open || cellAt(arm.x,arm.y + 1) == Empty) then
          if arm.state == Close then
            for y <- arm.y + 1 to 1 by -1 do
              matrix(arm.x)(y) = matrix(arm.x)(y - 1)
            matrix(arm.x)(0) = Position.Empty
          arm = arm.copy(y = arm.y + 1)
      case LEFT =>
        val canMove = arm.state match
          case Open =>
            cellAt(arm.x,arm.y) match
              case Empty =>
                arm.x > 0 && cellAt(arm.x - 1,arm.y) == Empty
              case _ =>
                false
          case Close =>
            cellAt(arm.x,arm.y) match
              case Empty =>
                arm.x > 0 && cellAt(arm.x - 1,arm.y) == Empty
              case _ =>
                cellAt(arm.x,arm.y + 1) == Empty && cellAt(arm.x - 1,arm.y + 1) == Empty && arm.y < 5

        if canMove then
          if arm.state == Close then
            for y <- 0 to arm.y do
              matrix(arm.x - 1)(y) = matrix(arm.x)(y)
              matrix(arm.x)(y) = Empty
          arm = arm.copy(x = arm.x - 1)
      case RIGHT =>
        val canMove = arm.state match
          case Open =>
            cellAt(arm.x,arm.y) match
              case Empty =>
                arm.x < 5 && cellAt(arm.x + 1,arm.y) == Empty
              case _ =>
                false
          case Close =>
            cellAt(arm.x,arm.y) match
              case Empty =>
                arm.x < 5 && cellAt(arm.x + 1,arm.y) == Empty
              case _ =>
                cellAt(arm.x,arm.y + 1) == Empty && cellAt(arm.x + 1,arm.y + 1) == Empty && arm.y < 5

        if canMove then
          if arm.state == Close then
            for y <- 0 to arm.y do
              matrix(arm.x + 1)(y) = matrix(arm.x)(y)
              matrix(arm.x)(y) = Empty
          arm = arm.copy(x = arm.x + 1)
      case OPEN =>
        cellGrabbed() match
          case Some(_) =>
            if arm.y < 5 && cellAt(arm.x,arm.y + 1) == Empty then
              var dropY = arm.y + 1
              while dropY < 6 && cellAt(arm.x,dropY) == Empty do dropY += 1
              dropY -= 1
              for y <- arm.y to 0 by -1 do
                matrix(arm.x)(dropY) = matrix(arm.x)(y)
                matrix(arm.x)(y) = Empty
                dropY -= 1
          case None =>
        arm = arm.copy(state = Open)
      case CLOSE =>
        arm = arm.copy(state = Close)
      case _ =>

    checkTarget()
    updateRenderer()

  private def checkTarget(): Unit =
    import Position.Empty
    lastMode = mem.read(MODE_ADDRESS)
    for y <- 0 to 5 ; x <- 0 to 4 do
      lastMode match
        case 1|2 =>
          if matrix(x)(y) != targetMatrix(x)(y) then return
        case 3 =>
          val source = matrix(x)(y)
          val target = targetMatrix(x)(y)
          if (source == Empty && target != Empty) || (source != Empty && target == Empty) then return
        case _ =>
          return

    // ok, match
    arm = arm.copy(state = ArmState.Open)
    //printMatrix(matrix)
    updateRenderer()
    
    val wait = (clk.getClockHz / 10).toInt // 100ms
    lastMode match
      case 1 =>
        controllerA.set(controllerA.START)
        clk.schedule(new ClockEvent("ROBController",clk.currentCycles + wait,_ => controllerA.clear(controllerA.START)))
      case 2 =>
        clk.schedule(new ClockEvent("ROBController",clk.currentCycles + 5 * clk.getClockHz.toInt,cycles => {
          controllerA.set(controllerA.START)
          clk.schedule(new ClockEvent("ROBController",cycles + wait,_ => controllerA.clear(controllerA.START)))
        }))
      case 3 =>
        clk.schedule(new ClockEvent("ROBController",clk.currentCycles + 5 * wait,cycles => {
          controllerA.set(controllerA.A)
          clk.schedule(new ClockEvent("ROBController", cycles + 20 * wait, cycles => {
            controllerA.clear(controllerA.A)
            controllerA.set(controllerA.START)
            clk.schedule(new ClockEvent("ROBController", cycles + wait, _ => controllerA.clear(controllerA.START)))
          }))
        }))
      case _ =>


  inline private def cellAt(x:Int,y:Int): Position =
    if x < 5 && y < 6 then
      matrix(x)(y)
    else Position.Empty


  inline private def cellGrabbed(): Option[Position] =
    arm.state match
      case ArmState.Close =>
        matrix(arm.x)(arm.y) match
          case Position.Empty =>
            None
          case c =>
            Some(c)
      case _ =>
        None

  private def printMatrix(matrix:Array[Array[Position]]): Unit =
    import ArmState.*
    import Position.*

    print("  ")
    for c <- 1 to 5 do print(s"| $c ")
    println("|")
    for y <- 0 until 6 do
      print("  ")
      for c <- 1 to 5 do print(s"+---")
      println("+")
      print(s"${y + 1} ")
      for x <- 0 until 5 do
        var cell = matrix(x)(y) match
          case Empty => " "
          case White => "W"
          case Blue => "B"
          case Yellow => "Y"
          case Green => "G"
          case Red => "R"
        if x == arm.x && y == arm.y then
          cell = arm.state match
            case Open => s"<$cell>"
            case Close => s">$cell<"
        else
          cell = s" $cell "
        print(s"|$cell")
        if x == 4 then println("|")
    print("  ")
    for c <- 1 to 5 do print(s"+---")
    println("+")
    updateRenderer()

  override def saveState(out:ObjectOutputStream): Unit =
    out.writeInt(lastMode)
    for x <- 0 until 5 ; y <- 0 until 5; do
      out.writeInt(matrix(x)(y).ordinal)

    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    lastMode = in.readInt()
    for x <- 0 until 5 ; y <- 0 until 5; do
      matrix(x)(y) = Position.fromOrdinal(in.readInt())

    super.loadState(in)  