package ucesoft.nes.util

import java.awt.Image
import java.awt.image.BufferedImage
import java.io.IOException

object ImageScaler {

  def resizeImage(originalImage: Image, targetWidth: Int, targetHeight: Int): BufferedImage = 
    val resultingImage = originalImage.getScaledInstance(targetWidth, targetHeight, Image.SCALE_DEFAULT)
    val outputImage = new BufferedImage(targetWidth, targetHeight, BufferedImage.TYPE_INT_RGB)
    outputImage.getGraphics.drawImage(resultingImage, 0, 0, null)
    outputImage
  
}
