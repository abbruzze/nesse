package ucesoft.nes.controller

import net.java.games.input.{Component, Controller, ControllerEnvironment}
import ucesoft.nes.Log

import java.awt.event.KeyEvent

object USBJoystick:
  class WaitButtonTask(controller:Controller,action: String => Unit) extends Runnable:
    private val thread = new Thread(this)
    @volatile private var stopped = false

    override def run(): Unit =
      while !stopped do
        Thread.sleep(100)
        controller.poll()
        val buttons = controller.getComponents filter { c => !c.isAnalog && !c.isRelative && c.getIdentifier().getClass().getName().toUpperCase().endsWith("BUTTON") }
        for c <- buttons do
          if c.getPollData != 0.0 && !stopped then
            action(c.getName)
            stopped = true

    def stop(): Unit = stopped = true
    def start(): Unit = thread.start()

  def getControllerNames() : List[String] =
    System.setProperty("jinput.loglevel","SEVERE")
    val controllers = getControllers()
    controllers filter { c => c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK } map { _.getName.trim } toList

  def waitForButton(controllerName:String,action: String => Unit): Option[WaitButtonTask] =
    ControllerEnvironment.getDefaultEnvironment.getControllers.find(_.getName.trim == controllerName.trim).map(c => WaitButtonTask(c,action))

  private[this] var controllers: Array[Controller] = null

  private def discoverControllers(): Unit = {
    controllers = Array()
    val thread = new Thread {
      override def run(): Unit = {
        System.setProperty("jinput.loglevel", "SEVERE")
        controllers = ControllerEnvironment.getDefaultEnvironment.getControllers
        Log.info(s"JInput controllers discovery terminated [${controllers.length}]")
      }
    }
    thread.start()
    thread.join(1000)
  }

  def getControllers(): Array[Controller] =
    if (controllers == null)
      discoverControllers()
    controllers

class USBJoystick(port:Int) extends Joystick(port) with Runnable:
  override val name = s"USB Joystick #$port"
  private case class Button(name:String,comp:Component,mask:Int)

  private[this] var controller : Option[Controller] = None
  private[this] var xAxisComponent : Component = _
  private[this] var yAxisComponent : Component = _
  private[this] var buttons : Array[Button] = _
  private[this] val dirThreshold = 0.5f
  private[this] var settings : USBControllerSettings = ControllerSettings.CONTROLLER_DEFAULT_USB_SETTINGS
  private[this] val thread = new Thread(this,s"USBJoystick$port")
  private[this] var running = false

  thread.start()
  USBJoystick.discoverControllers()

  override def setSettings(set: ControllerSettings): Unit =
    set match
      case key : USBControllerSettings =>
        settings = key
        Log.info("Finding USB controllers ...")
        findController()
      case _ =>

  private def findController() : Unit = {
    System.setProperty("jinput.loglevel","SEVERE")
    val controllers = USBJoystick.getControllers() //ControllerEnvironment.getDefaultEnvironment.getControllers
    controller = controllers find { c => c.getName.trim() == settings.deviceName && (c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK) }
    controller match {
      case None =>
      case Some(comp) =>
        Log.info(s"Found USB controller: ${comp.getName}")
        //println(s"Found: ${comp.getName}")
        xAxisComponent = comp.getComponent(Component.Identifier.Axis.X)
        yAxisComponent = comp.getComponent(Component.Identifier.Axis.Y)
        val searchButton = (name:String) => (comp.getComponents find { c =>
          !c.isAnalog && !c.isRelative && c.getIdentifier().getClass().getName().toUpperCase().endsWith("BUTTON") && c.getName == name
        }).orNull

        buttons = Array(Button("A",searchButton(settings.A),A),Button("B",searchButton(settings.B),B),Button("Select",searchButton(settings.Select),SELECT),Button("Start",searchButton(settings.Start),START))
        buttons = buttons filterNot { b => b.comp == null }

        for(b <- buttons) {
          Log.info(s"Found USB button ${b.name}")
        }
    }
  }

  def findButtonPressed() : Option[String] =
    if controller.isEmpty then findController()
    if controller.isEmpty || !controller.get.poll() then return None

    val buttons = controller.get.getComponents filter { c => !c.isAnalog && !c.isRelative && c.getIdentifier().getClass().getName().toUpperCase().endsWith("BUTTON") }
    for b <- buttons do
      if (b.getPollData != 0.0) return Some(b.getName)

    None

  override def run() =
    running = true
    while running do
      Thread.sleep(settings.pollingInMillis)

      data = 0
      controller match
        case None =>
          findController()
        case Some(c) if c.poll =>
          // fire
          var i = 0
          while i < buttons.length do
            if (buttons(i).comp.getPollData != 0.0)
              set(buttons(i).mask)
            i += 1

          val x = xAxisComponent.getPollData
          val y = yAxisComponent.getPollData

          if (x < -dirThreshold) set(LEFT)
          else if (x > dirThreshold) set(RIGHT)

          if (y < -dirThreshold) set(UP)
          else if (y > dirThreshold) set(DOWN)
        case _ =>
          controller = None

  override def keyTyped(e: KeyEvent): Unit = {}
  override def keyPressed(e: KeyEvent): Unit = {}
  override def keyReleased(e: KeyEvent): Unit = {}

  override def remove(): Unit =
    running = false