package ucesoft.nes.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.nes.{Cartridge, NES}

import java.awt.BorderLayout
import javax.swing.{ButtonGroup, JCheckBox, JPanel}

class GeneralPanel(nes:NES) extends JPanel {
  init()

  private def init(): Unit =
    import ucesoft.nes.misc.Preferences.*
    val pref = nes.preferences

    val regionAuto = new JCheckBox("Auto")
    val regionPAL = new JCheckBox("PAL")
    val regionNTSC = new JCheckBox("NTSC")
    val group = new ButtonGroup
    group.add(regionAuto)
    group.add(regionPAL)
    group.add(regionNTSC)

    val pauseIfFocusLost = new JCheckBox("Pause emulator when focus is lost")

    pref.get[String](PPU_TV_MODE).map(_.value) match {
      case None =>
        regionAuto.setSelected(true)
      case Some(reg) =>
        reg.toUpperCase() match {
          case "AUTO" => regionAuto.setSelected(true)
          case "PAL" => regionPAL.setSelected(true)
          case "NTSC" => regionNTSC.setSelected(true)
        }
    }

    pauseIfFocusLost.setSelected(pref.get[Boolean](PAUSE_IF_LOST_FOCUS).map(_.value).getOrElse(false))

    regionAuto.addActionListener(_ => nes.preferences.update[String](PPU_TV_MODE,"auto"))
    regionPAL.addActionListener(_ => nes.preferences.update[String](PPU_TV_MODE,"PAL"))
    regionNTSC.addActionListener(_ => nes.preferences.update[String](PPU_TV_MODE,"NTSC"))
    pauseIfFocusLost.addActionListener(_ => nes.preferences.update[Boolean](PAUSE_IF_LOST_FOCUS,pauseIfFocusLost.isSelected))

    val panel = FormBuilder.create().
      columns("5dlu,fill:pref,5dlu").
      rows("10dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,15dlu,pref,10dlu").
      addTitle("PPU Region").xy(2,2).
      add(regionAuto).xy(2,4).
      add(regionPAL).xy(2,6).
      add(regionNTSC).xy(2,8).
      add(pauseIfFocusLost).xy(2,10).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)
}

