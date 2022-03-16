package ucesoft.nes.controller
import java.awt.event.MouseEvent

class Zapper(port:Int,vsZapper:Boolean = false) extends Joystick(port) with OpticalDevice:
  override val name = "Zapper"
  override val inputType : InputType = InputType.Zapper

  private[this] final val LIGHT_SENSE = 0x08
  private[this] final val LIGHT_SENSE_SCAN_LINES = 26
  private[this] final val TRIGGER = 0x10
  private[this] var readState = 8
  private[this] var x,y = 0
  private[this] var threshold = 3
  private[this] var lightSenseCounter = 0
  private[this] var displayRatioX, displayRatioY = 0.0

  override def setSettings(set: ControllerSettings): Unit = {}

  if vsZapper then
    set(LEFT)
    set(UP)

  override def reset: Unit =
    lightSenseCounter = 0
    readState = 8
    if vsZapper then
      set(LEFT)
      set(UP)

  def setThreshold(t:Int): Unit = threshold = t
  def getThreshold: Int = threshold

  override def checkColor(colorX:Int,colorY:Int,color:Int): Unit =
    if colorX == 0 then
      if lightSenseCounter > 0 then
        lightSenseCounter -= 1
        if lightSenseCounter == 0 then
          readState |= LIGHT_SENSE
          if vsZapper then
            clear(LEFT)

    if colorX >= x - threshold && colorX <= x + threshold && colorY >= y - threshold && colorY <= y + threshold then
      if brightness(color) > WHITE_THRESHOLD then
        readState &= ~LIGHT_SENSE
        if vsZapper then
          set(LEFT)
        lightSenseCounter = LIGHT_SENSE_SCAN_LINES

  override def clock(): Unit =
    if vsZapper then
      super.clock()

  override def writePort(port: Int, value: Int): Unit =
    if vsZapper && port == 1 then
      super.writePort(port,value)

  override def readPort(port: Int): Int =
    if port == this.port then
      if vsZapper then
        super.readPort(port)
      else
        readState & 0x1F
    else 0

  override def mousePressed(e: MouseEvent): Unit =
    readState |= TRIGGER
    if vsZapper then
      set(RIGHT)

  override def mouseReleased(e: MouseEvent): Unit =
    readState &= ~TRIGGER
    if vsZapper then
      clear(RIGHT)

  override def mouseMoved(e: MouseEvent): Unit =
    x = (e.getX / displayRatioX).toInt
    y = (e.getY / displayRatioY).toInt

  override def mouseExited(e: MouseEvent): Unit = y = 0xFFFF

  override def displaySizeChanged(width: Int, height: Int, xRatio: Double, yRatio: Double): Unit =
    displayRatioX = xRatio
    displayRatioY = yRatio
