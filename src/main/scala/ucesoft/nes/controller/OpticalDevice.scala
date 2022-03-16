package ucesoft.nes.controller

trait OpticalDevice extends InputDevice:
  override val isOpticalDevice = true

  protected final val WHITE_THRESHOLD = 130

  // http://alienryderflex.com/hsp.html
  protected def brightness(color:Int): Double =
    val r = (color >> 16) & 0xFF
    val g = (color >> 8) & 0xFF
    val b = color & 0xFF
    math.sqrt(0.241 * r * r + 0.691 * g * g + 0.068 * b * b)
