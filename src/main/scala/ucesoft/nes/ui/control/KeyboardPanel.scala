package ucesoft.nes.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.nes.NES
import ucesoft.nes.controller.FamilyKeyboard
import ucesoft.nes.misc.Preferences
import ucesoft.nes.controller.{InputType, DeviceFactory}

import java.awt.{BorderLayout, FlowLayout}
import javax.swing.*

class KeyboardPanel(frame:JFrame,nes:NES) extends JPanel:
  private val layoutFile = new JTextField(30)

  init()

  private def init(): Unit =
    import ucesoft.nes.misc.Preferences.*
    val pref = nes.preferences

    val attachKeyb = new JCheckBox("Attach Family Keyboard")
    attachKeyb.setToolTipText("Modification on this setting will be applied on the next hard reset")

    val editor = new JButton("Keyboard layout editor ...")
    editor.addActionListener(_ => openEditor() )
    layoutFile.setEnabled(false)
    val browse = new JButton("Browse")
    val clearDefault = new JButton("Restore default")
    browse.addActionListener(_ => loadLayout() )
    clearDefault.addActionListener(_ => {
      pref.update(Preferences.KEYBOARD_LAYOUT_FILE,"")
      layoutFile.setText("")
    } )

    pref.get[Boolean](KEYBOARD_ON).map(_.value) match
      case Some(true) =>
        attachKeyb.setSelected(true)
      case _ =>
        browse.setEnabled(false)
        clearDefault.setEnabled(false)

    pref.get[String](KEYBOARD_LAYOUT_FILE).map(_.value) match
      case Some(f) if f.nonEmpty =>
        layoutFile.setText(f)
      case _ =>

    attachKeyb.addActionListener(_ => {
      pref.update(KEYBOARD_ON,attachKeyb.isSelected)
      browse.setEnabled(attachKeyb.isSelected)
      clearDefault.setEnabled(attachKeyb.isSelected)
    } )

    val panel = FormBuilder.create().
      columns("5dlu,right:pref,5dlu,pref,5dlu,pref,5dlu,pref,5dlu").
      rows("10dlu,pref,5dlu,pref,5dlu,pref,10dlu").
      add(attachKeyb).xy(2,1).
      addLabel("Layout:").xy(2,4).
      add(layoutFile).xy(4,4).
      add(browse).xy(6,4).
      add(clearDefault).xy(8,4).
      add(editor).xy(2,6).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)

  private def loadLayout(): Unit =
    val fc = new JFileChooser
    fc.showOpenDialog(this) match {
      case JFileChooser.APPROVE_OPTION =>
        FamilyKeyboard.loadLayout(fc.getSelectedFile.toString) match
          case Some(_) =>
            nes.preferences.update(Preferences.KEYBOARD_LAYOUT_FILE,fc.getSelectedFile.toString)
            layoutFile.setText(fc.getSelectedFile.toString)
      case _ =>
    }

  private def openEditor(): Unit =
    val editor = new KeyboardEditor(frame,DeviceFactory.keyboardLayout,newLayout => {
      nes.inputManager.getExpansionPortDevices().find(_.inputType == InputType.FamilyKeyboard) match
        case Some(keyb:FamilyKeyboard) =>
          keyb.layout = newLayout
        case None =>
    })
    editor.dialog.setVisible(true)


