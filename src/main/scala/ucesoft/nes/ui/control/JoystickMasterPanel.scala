package ucesoft.nes.ui.control

import ucesoft.nes.NES

import java.awt.BorderLayout
import javax.swing.JPanel

class JoystickMasterPanel(nes:NES) extends JPanel:

  init()

  private def init(): Unit =
    setLayout(new BorderLayout())
    add("North",JoystickPanel(1,nes))
    add("South",JoystickPanel(2,nes))