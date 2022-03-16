package ucesoft.nes.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.nes.controller.{ControllerSettings, KeyControllerSettings}
import ucesoft.nes.misc.Preferences
import ucesoft.nes.misc.Preferences.{JOY1_SETTINGS, JOY2_SETTINGS}

import java.awt.BorderLayout
import java.awt.event.{KeyAdapter, KeyEvent}
import javax.swing.{BorderFactory, JButton, JLabel, JPanel}

class JoystickKeysPanel(joyID:Int,pref:Preferences,k:KeyControllerSettings) extends JPanel {

  private class Label(text : String = "") extends JLabel(text) {
    var keyCode = 0
    def setText(text:String,code:Int): Unit = {
      super.setText(text)
      keyCode = code
    }
  }

  private val up = new JButton("Up")
  private val down = new JButton("Down")
  private val left = new JButton("Left")
  private val right = new JButton("Right")
  private val A = new JButton("A")
  private val B = new JButton("B")
  private val START = new JButton("Start")
  private val SELECT = new JButton("Select")
  private val buttons = Array(up,down,left,right,A,B,START,SELECT)
  private val upLabel = new Label
  private val downLabel = new Label
  private val leftLabel = new Label
  private val rightLabel = new Label
  private val aLabel,bLabel,startLabel,selectLabel = new Label

  init

  private def init : Unit = {
    import Preferences.*

    setBorder(BorderFactory.createTitledBorder(s"Keys settings #$joyID"))
    val panel = FormBuilder.create().
      columns("5dlu,fill:pref,2dlu,left:pref,80dlu").
      rows("10dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,10dlu").
      add(up).xy(2,2).add(upLabel).xy(4,2).
      add(down).xy(2,4).add(downLabel).xy(4,4).
      add(left).xy(2,6).add(leftLabel).xy(4,6).
      add(right).xy(2,8).add(rightLabel).xy(4,8).
      add(A).xy(2,10).add(aLabel).xy(4,10).
      add(B).xy(2,12).add(bLabel).xy(4,12).
      add(START).xy(2,14).add(startLabel).xy(4,14).
      add(SELECT).xy(2,16).add(selectLabel).xy(4,16).
      build()
    
    upLabel.setText(KeyEvent.getKeyText(k.UP),k.UP)
    downLabel.setText(KeyEvent.getKeyText(k.DOWN),k.DOWN)
    leftLabel.setText(KeyEvent.getKeyText(k.LEFT),k.LEFT)
    rightLabel.setText(KeyEvent.getKeyText(k.RIGHT),k.RIGHT)
    aLabel.setText(KeyEvent.getKeyText(k.A),k.A)
    bLabel.setText(KeyEvent.getKeyText(k.B),k.B)
    startLabel.setText(KeyEvent.getKeyText(k.Start),k.Start)
    selectLabel.setText(KeyEvent.getKeyText(k.Select),k.Select)

    up.addActionListener(_ => listenTo(upLabel))
    down.addActionListener(_ => listenTo(downLabel))
    left.addActionListener(_ => listenTo(leftLabel))
    right.addActionListener(_ => listenTo(rightLabel))
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

  private def listenTo(label:Label) : Unit = {
    label.setText("Press a key ...")
    requestFocus()
    for(b <- buttons) b.setEnabled(false)

    addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit = {
        for(b <- buttons) b.setEnabled(true)
        removeKeyListener(this)
        label.setText(KeyEvent.getKeyText(e.getKeyCode))
        label.keyCode = e.getKeyCode
        val settings = KeyControllerSettings(aLabel.keyCode,bLabel.keyCode,startLabel.keyCode,selectLabel.keyCode,upLabel.keyCode,downLabel.keyCode,leftLabel.keyCode,rightLabel.keyCode)
        pref.update(if joyID == 1 then JOY1_SETTINGS else JOY2_SETTINGS,ControllerSettings.settingsString(settings))
      }
    })
  }
}
