package ucesoft.nes.controller.rob

import ucesoft.nes.controller.Joystick
import ucesoft.nes.cpu.Memory

import java.awt.{BasicStroke, Color, Graphics, Graphics2D}
import java.awt.geom.{AffineTransform, Ellipse2D, GeneralPath, Line2D, RoundRectangle2D}
import java.io.{ObjectInputStream, ObjectOutputStream}
import javax.imageio.ImageIO
import javax.swing.{JComponent, JFrame, SwingUtilities, WindowConstants}

class ROBGyromiteController(override val mem:Memory,override val controllerA:Joystick,override val controllerB:Joystick) extends ROBController(mem,controllerA,controllerB):
  override protected type MatrixCell = Cell
  protected sealed trait Cell
  protected case object EmptyCell extends Cell
  protected case class Gyro(id:Int,var spinning:Boolean = false,var spinningTimer:Int = 0) extends Cell

  private[this] final val MAX_SPINNING_FRAME_COUNT = 3600
  override protected val matrix = Array.ofDim[Cell](5,3)
  private[this] val renderer = new ROBGyromiteRenderer
  private[this] val rendererFrame : JFrame = {
    val f = new JFrame("R.O.B.")
    f.getContentPane.add("Center",renderer)
    f.setSize(ROBGyromiteRenderer.WIDTH,ROBGyromiteRenderer.HEIGHT)
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    f.setIconImage(ImageIO.read(getClass.getResourceAsStream("/resources/images/nesse_logo.png")))
    f
  }

  private object ROBGyromiteRenderer:
    final val WIDTH = 800
    final val HEIGHT = 400

  private class ROBGyromiteRenderer extends JComponent {
    import ROBGyromiteRenderer.*

    private[this] val GYRO_POLY = Array(
      (1, 7), (1, 2), (6, 2), (6, 0), (1, 0), (1, -1), (0, -2),
      (-1, -1), (-1, 0), (-6, 0), (-6, 2), (-1, 2), (-1, 7)
    )
    private[this] val GYRO = createPoly(GYRO_POLY)
    private[this] val BASE_POLY = Array(
      (4, 0), (4, -1), (2, -1), (2, -3), (4, -3), (4, -7),
      (-4, -7), (-4, -3), (-2, -3), (-2, -1), (-4, -1), (-4, 0)
    )
    private[this] val BASE = createPoly(BASE_POLY)
    private[this] val SPINNER_POLY = Array(
      (2, 0), (2, -3), (4, -3), (4, -7),
      (-4, -7), (-4, -3), (-2, -3), (-2, 0)
    )
    private[this] val SPINNER = createPoly(SPINNER_POLY)
    private[this] val ARM_RIGHT_POLY = Array(
      (2, 6), (6, 6), (6, 4), (2, 4), (4, 5), (2, 6)
    )
    private[this] val ARM_RIGHT = createPoly(ARM_RIGHT_POLY)
    private[this] val ARM_LEFT_POLY = Array(
      (-2, 5), (-4, 4), (-6, 4), (-6, 6), (-4, 6)
    )
    private[this] val ARM_LEFT = createPoly(ARM_LEFT_POLY)
    private[this] val BUTTON_POLY = Array(
      (4, 0), (4, -2), (-4, -2), (-4, 0)
    )
    private[this] val BUTTON = createPoly(BUTTON_POLY)

    private def createPoly(points: Array[(Int, Int)]): GeneralPath =
      val path = new GeneralPath()
      path.moveTo(points(0)._1.toFloat, -points(0)._2.toFloat)
      for p <- points.drop(1) do
        path.lineTo(p._1.toFloat, -p._2.toFloat)
      path.closePath()
      path

    override def paint(g: Graphics): Unit =
      val g2 = g.asInstanceOf[Graphics2D]
      val dim = getSize()
      val at = new AffineTransform()
      val scaleT = new AffineTransform()
      val xscaleFactor = dim.width.toDouble / WIDTH * 10
      val yscaleFactor = dim.height.toDouble / HEIGHT * 10
      val ybase = dim.height / 2
      val yHeight = GYRO.getBounds.height * 0.7 * yscaleFactor
      scaleT.scale(xscaleFactor, yscaleFactor)
      val trackSize = dim.width / 6
      g2.setStroke(new BasicStroke(0.6))

      g2.setColor(Color.WHITE)
      g2.fillRect(0, 0, dim.width, dim.height)

      for x <- 0 to 4 do
        for y <- 0 to 2 do
          matrix(x)(y) match
            case g: Gyro =>
              at.setToIdentity()
              at.translate((x + 1) * trackSize, ybase + y * yHeight)
              at.concatenate(scaleT)
              g2.setTransform(at)
              g2.setColor(Color.BLACK)
              g2.draw(GYRO)
              g2.setColor(if g.spinning then Color.GRAY else Color.DARK_GRAY)
              g2.fill(GYRO)
            case _ =>
      for x <- 0 to 4 do
        at.setToIdentity()
        val offset = x match
          case 0 | 1 => 2
          case 2 =>
            if matrix(2)(2) != EmptyCell then 2.5 else 2
          case 3 =>
            if matrix(3)(2) != EmptyCell then 2.5 else 2
          case 4 => 1
        at.translate((x + 1) * trackSize, ybase + offset * yHeight)
        at.concatenate(scaleT)
        g2.setTransform(at)
        g2.setColor(Color.BLACK)
        x match
          case 0 | 1 =>
            g2.draw(BASE)
            g2.setColor(Color.ORANGE)
            g2.fill(BASE)
          case 2 =>
            g2.draw(BUTTON)
            g2.setColor(Color.BLUE)
            g2.fill(BUTTON)
          case 3 =>
            g2.draw(BUTTON)
            g2.setColor(Color.RED)
            g2.fill(BUTTON)
          case 4 =>
            g2.draw(SPINNER)
            g2.setColor(Color.ORANGE)
            g2.fill(SPINNER)

      val offset = if arm.state == ArmState.Open then 0 else 3
      at.setToIdentity()
      at.translate((arm.x + 1) * trackSize - xscaleFactor * offset, ybase + arm.y * yHeight)
      at.concatenate(scaleT)
      g2.setTransform(at)
      g2.setColor(Color.BLACK)
      g2.draw(ARM_RIGHT)
      g2.setColor(Color.GREEN)
      g2.fill(ARM_RIGHT)
      at.setToIdentity()
      at.translate((arm.x + 1) * trackSize + xscaleFactor * offset, ybase + arm.y * yHeight)
      at.concatenate(scaleT)
      g2.setTransform(at)
      g2.setColor(Color.BLACK)
      g2.draw(ARM_LEFT)
      g2.setColor(Color.GREEN)
      g2.fill(ARM_LEFT)

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

  // Constructor
  reset()

  override def getRenderer: JFrame = rendererFrame
  def updateRenderer(): Unit = renderer.repaint()

  override def onFrame(): Unit =
    def checkSpinningOf(x:Int,cell:Cell): Unit =
      def fall(g:Gyro): Unit =
        // fall
        matrix(x)(2) = EmptyCell
        handleButton(x,false)
        g.spinning = false
        if matrix(0)(2) == EmptyCell then
          matrix(0)(2) = g
        else
          if matrix(1)(2) == EmptyCell then
            matrix(1)(2) = g
          else
            if matrix(4)(1) == EmptyCell then
              matrix(4)(1) = g
        //printMatrix(matrix)
        SwingUtilities.invokeLater(() => updateRenderer())
      cell match
        case g@Gyro(_,spinning,_) =>
          if spinning then
            g.spinningTimer += 1
            if g.spinningTimer == MAX_SPINNING_FRAME_COUNT then
              fall(g)
          else
            cellGrabbed() match
              case Some(gy:Gyro)  =>
                if gy.id != g.id then fall(g)
              case _ =>
                fall(g)
        case _ =>

    checkSpinningOf(2,matrix(2)(2))
    checkSpinningOf(3,matrix(3)(2))

  override def reset(): Unit =
    super.reset()

    for c <- 0 until 5 ; r <- 0 until 3 do
      matrix(c)(r) = EmptyCell

    matrix(0)(2) = Gyro(1)
    matrix(1)(2) = Gyro(2)
    matrix(4)(1) = Gyro(3)

    //printMatrix(matrix)
    SwingUtilities.invokeLater(() => updateRenderer())

  override def getCommand(cmd: Int): Option[Command] =
    import Command.*
    cmd match
      case 0xBB => Some(UP)
      case 0xFB => Some(DOWN)
      case 0xBA => Some(LEFT)
      case 0xEA => Some(RIGHT)
      case 0xBE => Some(CLOSE)
      case 0xEE => Some(OPEN)
      case 0xEB => Some(TEST)
      case _ => None

  inline private def cellAt(x:Int,y:Int): Cell =
    if x < 5 && y < 3 then
      matrix(x)(y)
    else EmptyCell


  inline private def cellGrabbed(): Option[Cell] =
    arm.state match
      case ArmState.Close =>
        matrix(arm.x)(arm.y) match
          case EmptyCell =>
            None
          case c =>
            Some(c)
      case _ =>
        None

  override def executeCommand(cmd: Command): Unit =
    import ArmState.*
    import Command.*
    cmd match
      case TEST =>
        eyesOpen = true
      case UP =>
        if arm.y > 0 then
          if arm.state == Close then
            cellGrabbed() match
              case Some(gyro:Gyro) =>
                matrix(arm.x)(arm.y) = EmptyCell
                matrix(arm.x)(arm.y - 1) = gyro
                if (arm.x == 2 || arm.x == 3) && arm.y == 2 then
                  handleButton(arm.x,false)
                else if arm.x == 4 && arm.y == 1 then
                  gyro.spinning = true
                  gyro.spinningTimer = 0
              case None =>
          arm = arm.copy(y = arm.y - 1)
      case DOWN =>
        if arm.y < 2 && !(arm.x == 4 && arm.y == 1) then
          arm.state match
            case Open =>
              arm = arm.copy(y = arm.y + 1)
            case Close =>
              cellGrabbed() match
                case Some(gyro:Gyro) =>
                  if cellAt(arm.x,arm.y + 1) == EmptyCell && cellAt(arm.x,arm.y + 2) == EmptyCell then
                    matrix(arm.x)(arm.y) = EmptyCell
                    matrix(arm.x)(arm.y + 1) = gyro
                    if (arm.x == 2 || arm.x == 3) && arm.y == 1 then
                      handleButton(arm.x,true)
                    arm = arm.copy(y = arm.y + 1)
                case None =>
                  if cellAt(arm.x,arm.y + 1) == EmptyCell then
                    arm = arm.copy(y = arm.y + 1)
      case LEFT =>
        if arm.x > 0 then
          arm.state match
            case Open =>
              if cellAt(arm.x - 1,arm.y) == EmptyCell then
                arm = arm.copy(x = arm.x - 1)
            case Close =>
              cellGrabbed() match
                case Some(gyro:Gyro) =>
                  if arm.y != 2 &&
                    cellAt(arm.x - 1,arm.y) == EmptyCell &&
                    cellAt(arm.x - 1,arm.y + 1) == EmptyCell &&
                    !(arm.x == 4 && arm.y == 1) then
                    matrix(arm.x)(arm.y) = EmptyCell
                    matrix(arm.x - 1)(arm.y) = gyro
                    arm = arm.copy(x = arm.x - 1)
                case None =>
                  if cellAt(arm.x - 1,arm.y) == EmptyCell then
                    arm = arm.copy(x = arm.x - 1)
      case RIGHT =>
        if arm.x < 4 && !(arm.x == 3 && arm.y == 2) then
          arm.state match
            case Open =>
              if cellAt(arm.x + 1,arm.y) == EmptyCell then
                arm = arm.copy(x = arm.x + 1)
            case Close =>
              cellGrabbed() match
                case Some(gyro:Gyro) =>
                  if arm.y != 2 &&
                    !(arm.x == 3 && arm.y == 1) &&
                    cellAt(arm.x + 1,arm.y) == EmptyCell &&
                    cellAt(arm.x + 1,arm.y + 1) == EmptyCell then
                    matrix(arm.x)(arm.y) = EmptyCell
                    matrix(arm.x + 1)(arm.y) = gyro
                    arm = arm.copy(x = arm.x + 1)
                case None =>
                  if cellAt(arm.x + 1,arm.y) == EmptyCell then
                    arm = arm.copy(x = arm.x + 1)
      case CLOSE =>
        arm = arm.copy(state = Close)
      case OPEN =>
        cellGrabbed() match
          case None =>
            arm = arm.copy(state = Open)
          case Some(gyro:Gyro) =>
            if arm.x == 4 then
              matrix(arm.x)(arm.y) = EmptyCell
              matrix(arm.x)(1) = gyro
              arm = arm.copy(state = Open)
            else
              if cellAt(arm.x,arm.y + 2) == EmptyCell then
                matrix(arm.x)(arm.y) = EmptyCell
                matrix(arm.x)(2) = gyro
                if arm.x == 2 || arm.x == 3 then
                  handleButton(arm.x,true)
                arm = arm.copy(state = Open)
    //printMatrix(matrix)
    updateRenderer()

  private def handleButton(x:Int,push:Boolean): Unit =
    if x == 2 then
      if push then controllerB.set(controllerB.A) else controllerB.clear(controllerB.A)
    else
      if push then controllerB.set(controllerB.B) else controllerB.clear(controllerB.B)

  private def printMatrix(matrix:Array[Array[Cell]]): Unit =
    import ArmState.*

    print("  ")
    for c <- 1 to 5 do print(s"| $c ")
    println("|")
    for y <- 0 until 3 do
      print("  ")
      for c <- 1 to 5 do print(s"+---")
      println("+")
      print(s"${y + 1} ")
      for x <- 0 until 5 do
        var cell = matrix(x)(y) match
          case EmptyCell => " "
          case Gyro(id,_,_) => s"$id"
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
    for x <- 0 until 5 ; y <- 0 until 3; do
      matrix(x)(y) match
        case EmptyCell =>
          out.writeInt(0)
        case Gyro(id,spinning,spinningTimer) =>
          out.writeInt(id | (if spinning then 4 else 0) | spinningTimer << 5)

    super.saveState(out)

  override def loadState(in: ObjectInputStream): Unit =
    for x <- 0 until 5 ; y <- 0 until 3; do
      val cell = in.readInt()
      matrix(x)(y) = cell match
        case 0 => EmptyCell
        case v => Gyro(v & 3,(v & 4) > 0,v >> 5)

    super.loadState(in)