package ucesoft.nes.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.nes.NES
import ucesoft.nes.controller.{DeviceFactory, FamilyKeyboard, InputType}
import ucesoft.nes.misc.Preferences
import ucesoft.nes.mappers.fds.FDS

import java.awt.{BorderLayout, FlowLayout}
import java.io.File
import javax.swing.*

class FDSPanel(nes:NES) extends JPanel:
  private val romFile = new JTextField(40)

  init()

  private def init(): Unit =
    import ucesoft.nes.misc.Preferences.*
    val pref = nes.preferences

    romFile.setEditable(false)
    val browse = new JButton("Browse")
    browse.addActionListener(_ => selectROM() )

    val scrollLockEnabled = new JCheckBox("Use scroll lock key as disk access indicator")
    scrollLockEnabled.addActionListener(_ => {
      FDS.enableScrollLockAsDiskAccess(scrollLockEnabled.isSelected)
      nes.preferences.update(Preferences.FDS_DISK_SCROLL_LOCK_ENABLED,scrollLockEnabled.isSelected)
    })

    pref.get[Boolean](FDS_DISK_SCROLL_LOCK_ENABLED).map(_.value) match
      case Some(enabled) =>
        scrollLockEnabled.setSelected(enabled)
      case _ =>
        scrollLockEnabled.setSelected(false)

    pref.get[String](FDS_ROM_BIOS).map(_.value) match
      case Some(rom) if rom.nonEmpty =>
        romFile.setText(rom)
      case _ =>

    val panel = FormBuilder.create().
      columns("5dlu,right:pref,5dlu,pref,5dlu,pref,5dlu").
      rows("10dlu,pref,5dlu,pref,10dlu").
      addLabel("FDS ROM file:").xy(2,2).
      add(romFile).xy(4,2).
      add(browse).xy(6,2).
      add(scrollLockEnabled).xyw(2,4,5).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)

  private def selectROM(): Unit =
    val fc = new JFileChooser
    if !romFile.getText.isEmpty then
      val parent = new File(romFile.getText)
      if parent.getParentFile != null then
        fc.setCurrentDirectory(parent.getParentFile)
    fc.showOpenDialog(this) match {
      case JFileChooser.APPROVE_OPTION =>
        try
          FDS.setBiosROMFile(fc.getSelectedFile.toString)
          nes.preferences.update(Preferences.FDS_ROM_BIOS,fc.getSelectedFile.toString)
          romFile.setText(fc.getSelectedFile.toString)
        catch
          case i:IllegalArgumentException =>
            JOptionPane.showMessageDialog(this,i.getMessage,"FDS ROM loading error",JOptionPane.ERROR_MESSAGE)
      case _ =>
    }


