package ucesoft.nes.controller

import java.awt.event.{KeyEvent, KeyListener}

class KeyboardJoystick(port:Int) extends Joystick(port):
  override val name = s"Keyboard Joystick #$port"
  private[this] var settings : KeyControllerSettings = if port == 1 then ControllerSettings.CONTROLLER_1_DEFAULT_KEY_SETTINGS else ControllerSettings.CONTROLLER_2_DEFAULT_KEY_SETTINGS

  override def setSettings(set: ControllerSettings): Unit =
    set match
      case key:KeyControllerSettings => settings = key
      case _ =>

  override def keyTyped(e: KeyEvent): Unit = {}

  override def keyPressed(e: KeyEvent): Unit = {
    if enabled && !e.isAltDown then
      val k = e.getExtendedKeyCode
      if (k == settings.RIGHT) set(RIGHT)
      else if (k == settings.LEFT) set(LEFT)
      else if (k == settings.UP) set(UP)
      else if (k == settings.DOWN) set(DOWN)
      else if (k == settings.Start) set(START)
      else if (k == settings.Select) set(SELECT)
      else if (k == settings.A) set(A)
      else if (k == settings.B) set(B)
  }

  override def keyReleased(e: KeyEvent): Unit = {
    if enabled && !e.isAltDown then
      val k = e.getExtendedKeyCode
      if (k == settings.RIGHT) clear(RIGHT)
      else if (k == settings.LEFT) clear(LEFT)
      else if (k == settings.UP) clear(UP)
      else if (k == settings.DOWN) clear(DOWN)
      else if (k == settings.Start) clear(START)
      else if (k == settings.Select) clear(SELECT)
      else if (k == settings.A) clear(A)
      else if (k == settings.B) clear(B)
  }
