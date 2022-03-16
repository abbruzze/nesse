package ucesoft.nes.ui

import ucesoft.nes.misc.ScaledImageIcon

import java.io.File
import javax.swing.filechooser.FileView

object NESFileView extends FileView {
  private val icon = ScaledImageIcon("nesse_logo.png")

  override def getIcon(f:File) = {
    val name = f.getName.toUpperCase
    if (name.endsWith(".NES") || name.endsWith(".FDS")) icon
    else null
  }
}
