package ucesoft.nes.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.nes.NES

import java.awt.BorderLayout
import javax.swing.{JCheckBox, JPanel}

class VideoPanel(nes:NES) extends JPanel {
  init()

  private def init(): Unit =
    import ucesoft.nes.misc.Preferences.*
    val pref = nes.preferences

    val scanLineEffect = new JCheckBox("Scanline effect enabled")
    val overcanEnabled = new JCheckBox("Overscan enabled")
    val smoothRendering = new JCheckBox("Smooth rendering enabled")

    scanLineEffect.setSelected(pref.get[Boolean](PPU_SCANLINE_EFFECT).map(_.value).getOrElse(false))
    overcanEnabled.setSelected(pref.get[Boolean](PPU_OVERSCAN_ENABLED).map(_.value).getOrElse(false))
    smoothRendering.setSelected(pref.get[Boolean](PPU_SMOOTH_RENDERING).map(_.value).getOrElse(false))

    scanLineEffect.addActionListener(_ => nes.preferences.update[Boolean](PPU_SCANLINE_EFFECT,scanLineEffect.isSelected))
    overcanEnabled.addActionListener(_ => nes.preferences.update[Boolean](PPU_OVERSCAN_ENABLED,overcanEnabled.isSelected))
    smoothRendering.addActionListener(_ => nes.preferences.update[Boolean](PPU_SMOOTH_RENDERING,smoothRendering.isSelected))

    val panel = FormBuilder.create().
      columns("5dlu,fill:pref,5dlu").
      rows("10dlu,pref,5dlu,pref,5dlu,pref,10dlu").
      add(scanLineEffect).xy(2,2).
      add(overcanEnabled).xy(2,4).
      add(smoothRendering).xy(2,6).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)
}

