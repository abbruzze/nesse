package ucesoft.nes.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.nes.controller.{ControllerSettings, USBControllerSettings, USBJoystick}
import ucesoft.nes.misc.Preferences
import ucesoft.nes.misc.Preferences.{JOY1_SETTINGS, JOY2_SETTINGS}

import java.awt.BorderLayout
import java.awt.event.{KeyAdapter, KeyEvent}
import java.util.Properties
import javax.swing.*

class JoystickUSBPanel(joyID:Int, pref:Preferences, k:USBControllerSettings) extends JPanel {
  private val controllers = new JComboBox[String](USBJoystick.getControllerNames().toArray)
  private val A = new JButton("A")
  private val B = new JButton("B")
  private val START = new JButton("Start")
  private val SELECT = new JButton("Select")
  private val buttons = Array(A,B,START,SELECT)
  private val aLabel,bLabel,startLabel,selectLabel = new JLabel
  private val pollingTime = new JTextField(k.pollingInMillis.toString,5)

  init

  private def init : Unit = {
    import Preferences.*

    setBorder(BorderFactory.createTitledBorder(s"${k.deviceName} settings #$joyID"))
    val panel = FormBuilder.create().
      columns("5dlu,fill:pref,2dlu,left:pref,80dlu").
      rows("10dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,10dlu").
      add("USB controller").xy(2,2).add(controllers).xy(4,2).
      add("Polling time (millis):").xy(2,4).add(pollingTime).xy(4,4).
      add(A).xy(2,6).add(aLabel).xy(4,6).
      add(B).xy(2,8).add(bLabel).xy(4,8).
      add(START).xy(2,10).add(startLabel).xy(4,10).
      add(SELECT).xy(2,12).add(selectLabel).xy(4,12).
      build()

    val selected = USBJoystick.getControllerNames().toArray.indexOf(k.deviceName)
    if selected != -1 then
      controllers.setSelectedIndex(selected)
      
    aLabel.setText(k.A)
    bLabel.setText(k.B)
    startLabel.setText(k.Start)
    selectLabel.setText(k.Select)

    A.addActionListener(_ => listenTo(aLabel))
    B.addActionListener(_ => listenTo(bLabel))
    START.addActionListener(_ => listenTo(startLabel))
    SELECT.addActionListener(_ => listenTo(selectLabel))

    setLayout(new BorderLayout())
    add("Center",panel)

    setFocusable(true)
  }

  override def setEnabled(enabled: Boolean): Unit = {
    for(b <- buttons) b.setEnabled(enabled)
  }

  private def listenTo(label:JLabel) : Unit = {
    USBJoystick.waitForButton(controllers.getSelectedItem.toString,buttonPressed => {
      label.setText(buttonPressed)
      controllers.setEnabled(true)
      for(b <- buttons) b.setEnabled(true)
      val pt = try
        pollingTime.getText.toInt
      catch
        case _:NumberFormatException => 10
      val settings = USBControllerSettings(aLabel.getText,bLabel.getText,startLabel.getText,selectLabel.getText,controllers.getSelectedItem.toString,pt)
      pref.update(if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS,ControllerSettings.settingsString(settings))
    }) match {
      case Some(wb) =>
        val oldText = label.getText
        label.setText("Press a joystick button or ESC to abort...")
        addKeyListener(new KeyAdapter {
          override def keyPressed(e: KeyEvent): Unit = {
            if e.getKeyCode == KeyEvent.VK_ESCAPE then
              wb.stop()
              controllers.setEnabled(true)
              for(b <- buttons) b.setEnabled(true)
              removeKeyListener(this)
              label.setText(oldText)
          }
        })
        requestFocus()
        controllers.setEnabled(false)
        for(b <- buttons) b.setEnabled(false)
        wb.start()
      case None =>
    }
   
  }
}
