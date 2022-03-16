package ucesoft.nes.controller
import ucesoft.nes.util.CartDB.VSSwitches
import ucesoft.nes.{Clock, ClockEvent, Display, NESComponent, NESComponentType}

import java.awt.event.{KeyEvent, MouseEvent, MouseListener, MouseMotionListener}
import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties
import scala.collection.mutable.ListBuffer

class InputManager extends NESComponent with InputDevice with MouseListener with MouseMotionListener with Display.DisplaySizeChangedListener:
  override val name = "InputManager"
  override val componentID: String = "Input Manager"
  override val componentType: NESComponentType = NESComponentType.CHIP
  override val inputType: InputType = InputType.Manager

  private[this] inline val INSERT_COIN_CYCLES = 1_789_772 * 60 / 1000 // up for 60ms
  private[this] inline val SERVICE_CYCLES = 1_789_772 // up for 1s
  private[this] inline val BLUE_GREEN_BUTTON_CYCLES = SERVICE_CYCLES / 2

  val controllers : Array[Joystick] = Array(new KeyboardJoystick(1),new KeyboardJoystick(2))
  private[this] val expansionPortDevices : ListBuffer[InputDevice] = new collection.mutable.ListBuffer[InputDevice]()
  // VS
  private[this] var isVS = false
  private[this] var vsSwitches = 0
  private[this] var vsSwitchList: List[VSSwitches] = Nil
  private[this] var insertCoinStatus,serviceStatus = 0
  private[this] var swapControllers = false

  def setVS(vs:Boolean,switches:List[VSSwitches] = Nil,_swapControllers:Boolean = false):Unit =
    isVS = vs
    vsSwitches = 0
    vsSwitchList = switches
    swapControllers = _swapControllers
    if !switches.isEmpty then
      vsSwitches = switches.map(s => s.switches(s.activeIndex).switch).reduce(_ | _)

  def pressInsertCoin():Unit =
    insertCoinStatus = 0x20
    val clk = Clock.systemClock
    clk.schedule(new ClockEvent("VSInsertCoin",clk.currentCycles + INSERT_COIN_CYCLES,_ => insertCoinStatus = 0))

  def pressService():Unit =
    serviceStatus = 0x04
    val clk = Clock.systemClock
    clk.schedule(new ClockEvent("VSService",clk.currentCycles + SERVICE_CYCLES,_ => serviceStatus = 0))

  def pressBlueOrGreenButton(blue:Boolean):Unit =
    val c = if blue then
      if swapControllers then 1 else 0
    else
      if swapControllers then 0 else 1
    controllers(c).set(controllers(c).SELECT)
    val clk = Clock.systemClock
    clk.schedule(new ClockEvent("VSBlueButton",clk.currentCycles + BLUE_GREEN_BUTTON_CYCLES,_ => controllers(c).clear(controllers(c).SELECT)))

  override def getProperties: Properties =
    val p = new Properties()
    for c <- controllers do
      p.setProperty(c.name,"")
    for c <- expansionPortDevices do
      p.setProperty(c.name,"")
    p

  def addExpansionPortDevice(device:InputDevice) : Unit = expansionPortDevices += device
  def removeExpansionPortDevice(device:InputDevice) : Unit =
    expansionPortDevices -= device
    device.eject()
  def removeAllExpansionPortDevices() : Unit =
    for d <- expansionPortDevices do
      d.eject()
    expansionPortDevices.clear()
  def getExpansionPortDevices() : List[InputDevice] = expansionPortDevices.toList

  override def clock(): Unit =
    controllers(0).clock()
    controllers(1).clock()
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).clock()
      d += 1

  override def readPort(port: Int): Int =
    // VS
    var read = if isVS then
      if port == 1 then
        insertCoinStatus | serviceStatus | (vsSwitches & 3) << 3
      else (vsSwitches & 0xFC)
    else 0x40

    val controllerPort = if swapControllers then
      if port == 1 then 2 else 1
    else port
    read |= controllers(controllerPort - 1).readPort(controllerPort)
    // Expansion
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      read |= expansionPortDevices(d).readPort(port)
      d += 1

    read

  override def writePort(port: Int, value: Int): Unit =
    controllers(0).writePort(port,value)
    controllers(1).writePort(port,value)
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).writePort(port,value)
      d += 1

  override def keyTyped(e: KeyEvent): Unit = {}

  override def keyPressed(e: KeyEvent): Unit =
    controllers(0).keyPressed(e)
    controllers(1).keyPressed(e)
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).keyPressed(e)
      d += 1

  override def keyReleased(e: KeyEvent): Unit =
    controllers(0).keyReleased(e)
    controllers(1).keyReleased(e)
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).keyReleased(e)
      d += 1

  override def mouseClicked(e: MouseEvent): Unit =
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).mouseClicked(e)
      d += 1
  override def mousePressed(e: MouseEvent): Unit =
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).mousePressed(e)
      d += 1
  override def mouseReleased(e: MouseEvent): Unit =
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).mouseReleased(e)
      d += 1
  override def mouseEntered(e: MouseEvent): Unit =
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).mouseEntered(e)
      d += 1
  override def mouseExited(e: MouseEvent): Unit =
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).mouseExited(e)
      d += 1
  override def mouseDragged(e: MouseEvent): Unit = {}
  override def mouseMoved(e: MouseEvent): Unit =
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).mouseMoved(e)
      d += 1

  override def displaySizeChanged(width: Int, height: Int, xRatio: Double, yRatio: Double): Unit =
    var d = 0
    val size = expansionPortDevices.size
    while d < size do
      expansionPortDevices(d).displaySizeChanged(width,height, xRatio, yRatio)
      d += 1

  override def reset: Unit =
    controllers(0).reset
    controllers(1).reset
    for(d <- expansionPortDevices) d.reset

  override def init: Unit = {}

  override def saveState(out: ObjectOutputStream): Unit =
    for d <- expansionPortDevices do
      d.saveDeviceState(out)

  override def loadState(in: ObjectInputStream): Unit =
    for d <- expansionPortDevices do
      d.loadDeviceState(in)
