package ucesoft.nes.ui.control

trait VideoControl {
  def zoomDisplay(factor:Int) : Unit
  def aspect4_3() : Unit
  def fullScreen() : Unit
}
