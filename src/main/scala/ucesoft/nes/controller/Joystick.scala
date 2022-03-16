package ucesoft.nes.controller

import ucesoft.nes.{NESComponent, NESComponentType}

import java.awt.event.KeyListener
import java.util.Properties

/**
 *_____________________________________
 |    _				                        |
 |  _| |_ 		            Nintendo    |
 | |_   _| SELECT START		            |
 |   |_|    (==)  (==)   ( B ) ( A )  |
 |____________________________________|
 */
abstract class Joystick(val port:Int) extends NESComponent with InputDevice {
  override val inputType: InputType = InputType.Controller

  override val componentType: NESComponentType = NESComponentType.JOYSTICK
  override val componentID: String = s"Joystick #$port"

  final val RIGHT   = 0x1
  final val LEFT    = 0x2
  final val DOWN    = 0x4
  final val UP      = 0x8
  final val START   = 0x10
  final val SELECT  = 0x20
  final val B       = 0x40
  final val A       = 0x80

  override def getProperties: Properties = {
    properties.setProperty("Data",data.toString)
    properties
  }

  @volatile protected var data = 0
  protected var latch = 0
  protected var latchMode = false
  protected var readCount = 0

  def setSettings(set:ControllerSettings) : Unit
  
  def remove(): Unit = {}
  
  override def init : Unit = {}
  override def reset : Unit =
    data = 0
    latch = 0
    latchMode = false

  def set(bit:Int) : Unit =
    data |= bit
  def clear(bit:Int) : Unit =
    data &= ~bit
  
  inline private def setLatchMode(enabled:Boolean) : Unit = latchMode = enabled

  def isLatchMode: Boolean = latchMode

  override def clock() : Unit =
    if latchMode then
      latch = data
      readCount = 8

  inline private def shiftedData() : Int =
    if readCount > 0 then
      val d = ((latch & 0x80) >> 7) & 0x1
      readCount -= 1
      latch <<= 1
      d
    else 1

  override def writePort(port: Int, value: Int): Unit =
    if port == 1 then setLatchMode((value & 1) > 0)

  override def readPort(port: Int): Int =
    if port == this.port then shiftedData() else 0
}