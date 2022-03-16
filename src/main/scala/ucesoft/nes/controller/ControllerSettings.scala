package ucesoft.nes.controller

sealed trait ControllerSettings
case class KeyControllerSettings(A:Int,B:Int,Start:Int,Select:Int,UP:Int,DOWN:Int,LEFT:Int,RIGHT:Int) extends ControllerSettings
case class USBControllerSettings(A:String,B:String,Start:String,Select:String,deviceName:String,pollingInMillis:Int) extends ControllerSettings

object ControllerSettings:
  import java.awt.event.KeyEvent.*
  // default key settings for #1 and #2
  val CONTROLLER_1_DEFAULT_KEY_SETTINGS = KeyControllerSettings(A = VK_D,B = VK_A, Start = VK_ENTER, Select = VK_CONTROL, UP = VK_UP, DOWN = VK_DOWN, LEFT = VK_LEFT, RIGHT = VK_RIGHT)
  val CONTROLLER_2_DEFAULT_KEY_SETTINGS = KeyControllerSettings(A = VK_T,B = VK_U, Start = VK_P, Select = VK_O, UP = VK_I, DOWN = VK_M, LEFT = VK_J, RIGHT = VK_L)
  val CONTROLLER_DEFAULT_USB_SETTINGS = USBControllerSettings(A = "Button 0", B = "Button 1",Select = "Button 2", Start = "Button 3",deviceName = "USB Joystick",10)

  def settingsString(s:ControllerSettings): String =
    s match
      case KeyControllerSettings(a,b,start,select,up,down,left,right) =>
        s"key:$a,$b,$start,$select,$up,$down,$left,$right"
      case USBControllerSettings(a,b,start,select,devName,polling) =>
        s"usb:$a,$b,$start,$select,$devName,$polling"
      case _ =>
        ""

  def parseSettings(s:String): Option[ControllerSettings] =
    s.split(":") match
      case Array("key",set) =>
        set.split(",") match
          case Array(a,b,start,select,up,down,left,right) =>
            Some(KeyControllerSettings(a.toInt,b.toInt,start.toInt,select.toInt,up.toInt,down.toInt,left.toInt,right.toInt))
          case _ =>
            None
      case Array("usb",set) =>
        set.split(",") match
          case Array(a,b,start,select,devName,polling) =>
            Some(USBControllerSettings(a,b,start,select,devName,polling.toInt))
          case _ =>
            None
      case _ =>
        None