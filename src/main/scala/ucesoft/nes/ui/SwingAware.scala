package ucesoft.nes.ui

import javax.swing.SwingUtilities

trait SwingAware {
  def swing(action : => Unit) : Unit = SwingUtilities.invokeLater(() => action)
}
