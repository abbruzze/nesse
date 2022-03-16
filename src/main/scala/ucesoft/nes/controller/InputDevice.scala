package ucesoft.nes.controller

import ucesoft.nes.Display

import java.awt.event.{KeyEvent, KeyListener, MouseEvent, MouseListener, MouseMotionListener}
import java.io.{ObjectInputStream, ObjectOutputStream}

enum InputType(val id:Int,val nameType:String):
  case Manager extends InputType(0xFF,"")
  case Controller extends InputType(0x00,"Controller")
  case FamilyKeyboard extends InputType(0x23,"familykeyboard")
  case Zapper extends InputType(0x08,"zapper")
  case VSZapper extends InputType(0x07,"vszapper")
  case ROB extends InputType(0x1F,"rob")

trait InputDevice extends KeyListener with MouseListener with MouseMotionListener with Display.DisplaySizeChangedListener {
  val name : String
  val inputType : InputType
  val isOpticalDevice = false

  var enabled : Boolean = true

  def readPort(port:Int) : Int
  def writePort(port:Int,value:Int) : Unit

  def reset : Unit = {}
  
  def eject(): Unit = {}
  
  def clock(): Unit = {}

  override def keyTyped(e: KeyEvent): Unit = {}
  override def keyPressed(e: KeyEvent): Unit = {}
  override def keyReleased(e: KeyEvent): Unit = {}
  override def mouseClicked(e: MouseEvent): Unit = {}
  override def mousePressed(e: MouseEvent): Unit = {}
  override def mouseReleased(e: MouseEvent): Unit = {}
  override def mouseEntered(e: MouseEvent): Unit = {}
  override def mouseExited(e: MouseEvent): Unit = {}
  override def mouseDragged(e: MouseEvent): Unit = {}
  override def mouseMoved(e: MouseEvent): Unit = {}

  override def displaySizeChanged(width: Int, height: Int, xRatio: Double, yRatio: Double): Unit = {}

  def checkColor(colorX:Int,colorY:Int,color:Int): Unit = {}

  def saveDeviceState(out:ObjectOutputStream): Unit = {}
  def loadDeviceState(in:ObjectInputStream): Unit = {}
}
