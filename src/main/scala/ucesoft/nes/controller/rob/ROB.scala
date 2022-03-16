package ucesoft.nes.controller.rob

import ucesoft.nes.NESComponent
import ucesoft.nes.controller.{InputType, Joystick, OpticalDevice}
import ucesoft.nes.cpu.Memory

import java.io.{ObjectInputStream, ObjectOutputStream}
import javax.swing.JFrame

abstract class ROBController(val mem:Memory,val controllerA:Joystick,val controllerB:Joystick):
  protected type MatrixCell

  protected enum ArmState:
    case Open, Close
  protected case class Arm(x:Int,y:Int,state:ArmState)

  protected var arm = Arm(2,0,ArmState.Open)
  protected val matrix : Array[Array[MatrixCell]]
  var eyesOpen = false

  enum Command:
    case UP,DOWN,LEFT,RIGHT,OPEN,CLOSE,TEST

  def getCommand(cmd:Int) : Option[Command]
  def reset(): Unit =
    eyesOpen = false
    arm = Arm(2,0,ArmState.Open)

  def executeCommand(cmd:Command): Unit

  def onFrame(): Unit = {}

  def getRenderer: JFrame

  def updateRenderer(): Unit

  def saveState(out:ObjectOutputStream): Unit =
    out.writeInt(arm.x)
    out.writeInt(arm.y)
    out.writeInt(arm.state.ordinal)

  def loadState(in:ObjectInputStream): Unit =
    val x = in.readInt()
    val y = in.readInt()
    val s = ArmState.fromOrdinal(in.readInt())
    arm = arm.copy(x = x,y = y,state = s)
    updateRenderer()

class ROB(controller:ROBController) extends OpticalDevice:
  override val name = "ROB"
  override val inputType: InputType = InputType.ROB
  private[this] var waitingCommand = true
  private[this] var frameBrightness = 0.0
  private[this] var shifter = 0
  private[this] var commandBitCount = 0
  private[this] var eyesCounter = 0

  // Constructor
  controller.getRenderer.setVisible(true)

  override def reset: Unit =
    waitingCommand = true
    frameBrightness = 0.0
    shifter = 0
    commandBitCount = 0
    eyesCounter = 0
    controller.reset()

  override def eject(): Unit =
    controller.getRenderer.setVisible(false)

  override def checkColor(colorX: Int, colorY: Int, color: Int): Unit =
    if colorX == 0 && colorY == 0 then
      checkBit(frameBrightness / (128 * 128))
      frameBrightness = 0.0
      controller.onFrame()
      if controller.eyesOpen then
        eyesCounter += 1
        if eyesCounter == 120 then
          eyesCounter = 0
          controller.eyesOpen = false
          controller.updateRenderer()

    // 128 x 128 box in the centre
    if colorX >= 64 && colorX < 192 && colorY >= 56 && colorY < 184 then
      frameBrightness = frameBrightness + brightness(color)

  private def checkBit(brightness:Double): Unit =
    val bit = if brightness > 130 then 1 else 0
    if waitingCommand then
      commandBitCount match
        case 0 if bit == 0 => commandBitCount = 1
        case 1 if bit == 0 => commandBitCount = 2
        case 2 if bit == 0 => commandBitCount = 3
        case 3 if bit == 1 => commandBitCount = 4
        case 3 if bit == 0 =>
        case 4 if bit == 0 =>
          waitingCommand = false
          shifter = 0
          commandBitCount = 0
        case _ => commandBitCount = 0
    else
      shifter = (shifter << 1) | bit
      commandBitCount += 1
      if commandBitCount == 8 then
        issueCommand(shifter)
        waitingCommand = true
        commandBitCount = 0

  private def issueCommand(cmd:Int): Unit =
    controller.getCommand(cmd) match
      case Some(c) =>
        controller.executeCommand(c)
      case None =>
        println("COMMAND UNKNOWN: " + cmd)

  override def writePort(port: Int, value: Int): Unit = {}

  override def readPort(port: Int): Int = 0

  override def saveDeviceState(out:ObjectOutputStream): Unit = controller.saveState(out)
  override def loadDeviceState(in: ObjectInputStream): Unit = controller.loadState(in)