package ucesoft.nes.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.nes.NES
import ucesoft.nes.controller.{ControllerSettings, KeyControllerSettings, USBControllerSettings, USBJoystick}

import java.awt.{BorderLayout, Color}
import javax.swing.{BorderFactory, ButtonGroup, JButton, JCheckBox, JDialog, JFrame, JLabel, JPanel, JRadioButton}

class JoystickPanel(joyID:Int,nes:NES) extends JPanel:
  import ucesoft.nes.misc.Preferences.*

  init()

  private def init(): Unit =
    val pref = nes.preferences
    val conf = nes.configuration

    setBorder(BorderFactory.createTitledBorder(s"Controller #$joyID settings"))
    val keyJoy = new JRadioButton("Keyboard")
    val usbJoy = new JRadioButton("USB Joystick")
    val group = new ButtonGroup
    group.add(keyJoy)
    group.add(usbJoy)
    val warningLabel = if USBJoystick.getControllerNames().size == 0 then
      val l = new JLabel("<html>Warning: no USB joystick found.<br>Please, restart the emulator if you have attached one.<html>")
      l.setForeground(Color.RED)
      usbJoy.setEnabled(false)
      l
    else new JLabel()

    val attachZapper = new JCheckBox("Attach Zapper")
    attachZapper.setToolTipText("Modification on this setting will be applied on the next hard reset")

    pref.get[String](ZAPPER_ON_PORT).map(_.value) match
      case Some("none") =>
      case Some(p) if p.toInt == joyID =>
        attachZapper.setSelected(true)
      case _ =>
        
    attachZapper.addActionListener(_ => {})

    val keySet = new JButton("Keyboard settings")
    val usbSet = new JButton("USB settings")

    keyJoy.addActionListener(_ => {
      import ControllerSettings.*
      keySet.setEnabled(true)
      usbSet.setEnabled(false)
      conf.setProperty(s"LAST_JOY_${joyID}_USB_CONF",pref.get[String](if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS).map(_.value).get)
      nes.configuration.getProperty(s"LAST_JOY_${joyID}_KEY_CONF") match
        case null =>
          nes.preferences.update(if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS,
            settingsString(if joyID == 1 then CONTROLLER_1_DEFAULT_KEY_SETTINGS else CONTROLLER_2_DEFAULT_KEY_SETTINGS))
        case set =>
          nes.preferences.update(if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS,set)
    })
    usbJoy.addActionListener(_ => {
      import ControllerSettings.*
      keySet.setEnabled(false)
      usbSet.setEnabled(true)
      conf.setProperty(s"LAST_JOY_${joyID}_KEY_CONF",pref.get[String](if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS).map(_.value).get)
      nes.configuration.getProperty(s"LAST_JOY_${joyID}_USB_CONF") match
        case null =>
          nes.preferences.update(if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS,settingsString(CONTROLLER_DEFAULT_USB_SETTINGS))
        case set =>
          nes.preferences.update(if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS,set)
    })

    keySet.addActionListener(_ => {
      val dialog = new JDialog(javax.swing.FocusManager.getCurrentManager().getActiveWindow(),s"Controller #$joyID - Keyboard settings",java.awt.Dialog.ModalityType.APPLICATION_MODAL)
      dialog.getContentPane.add("Center",new JoystickKeysPanel(joyID,nes.preferences,keySettings()))
      dialog.setLocationRelativeTo(javax.swing.FocusManager.getCurrentManager().getActiveWindow())
      dialog.setResizable(false)
      dialog.pack()
      dialog.setVisible(true)
    })
    usbSet.addActionListener(_ => {
      val dialog = new JDialog(javax.swing.FocusManager.getCurrentManager().getActiveWindow(),s"Controller #$joyID - USB settings",java.awt.Dialog.ModalityType.APPLICATION_MODAL)
      dialog.getContentPane.add("Center",new JoystickUSBPanel(joyID,nes.preferences,usbSettings()))
      dialog.setLocationRelativeTo(javax.swing.FocusManager.getCurrentManager().getActiveWindow())
      dialog.setResizable(false)
      dialog.pack()
      dialog.setVisible(true)
    })


    pref.get[String](if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS).map(_.value) match
      case Some(conf) =>
        ControllerSettings.parseSettings(conf) match
          case Some(k:KeyControllerSettings) =>
            keyJoy.setSelected(true)
            usbSet.setEnabled(false)
          case Some(u:USBControllerSettings) =>
            usbJoy.setSelected(true)
            keySet.setEnabled(false)
          case None =>

    val panel = FormBuilder.create().
      columns("5dlu,fill:pref,5dlu,fill:pref,5dlu").
      rows("10dlu,pref,30dlu,pref,10dlu,pref,10dlu,pref,20dlu").
      addStack(keyJoy,usbJoy).xy(2,2).add(warningLabel).xy(4,2).
      add(keySet).xy(2,4).
      add(usbSet).xy(2,6).
      add(attachZapper).xy(2,8).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)

  private def keySettings(): KeyControllerSettings =
    import ControllerSettings.*
    parseSettings(nes.preferences.get[String](if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS).map(_.value).get) match
      case Some(k:KeyControllerSettings) => k
      case _ =>
        val confName = s"LAST_JOY_${joyID}_KEY_CONF"
        nes.configuration.getProperty(confName) match
          case null =>
            if joyID == 1 then CONTROLLER_1_DEFAULT_KEY_SETTINGS else CONTROLLER_2_DEFAULT_KEY_SETTINGS
          case settings =>
            parseSettings(settings) match
              case Some(s:KeyControllerSettings) => s
              case None =>
                // error
                if joyID == 1 then CONTROLLER_1_DEFAULT_KEY_SETTINGS else CONTROLLER_2_DEFAULT_KEY_SETTINGS

  private def usbSettings(): USBControllerSettings =
    import ControllerSettings.*
    parseSettings(nes.preferences.get[String](if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS).map(_.value).get) match
      case Some(k:USBControllerSettings) => k
      case _ =>
        val confName = s"LAST_JOY_${joyID}_USB_CONF"
        nes.configuration.getProperty(confName) match
          case null =>
            CONTROLLER_DEFAULT_USB_SETTINGS
          case settings =>
            parseSettings(settings) match
              case Some(s:USBControllerSettings) => s
              case _ =>
                // error
                CONTROLLER_DEFAULT_USB_SETTINGS