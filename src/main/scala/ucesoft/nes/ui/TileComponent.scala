package ucesoft.nes.ui

import java.awt.event.{MouseEvent, MouseMotionListener}
import java.awt.*
import javax.swing.{JComponent, JLabel, JPanel}

class TileComponent(var image:Image, cursorSize:Int) extends JComponent {
  private var cursorX, cursorY = 0

  private val _size = image.getWidth(null) / cursorSize
  private val coordLabel = new JLabel("0,0")

  private class Canvas extends JComponent with MouseMotionListener:
    setPreferredSize(new Dimension(image.getWidth(null),image.getHeight(null)))
    addMouseMotionListener(this)

    override def paint(g: Graphics): Unit =
      g.drawImage(image,0,0,null)
      g.setColor(Color.WHITE)
      g.drawRect(cursorX * _size,cursorY * _size,_size,_size)

    override def mouseDragged(e: MouseEvent): Unit = {}
    override def mouseMoved(e: MouseEvent): Unit =
      cursorX = e.getX / _size
      cursorY = e.getY / _size
      coordLabel.setText(s"$cursorX,$cursorY : ${cursorY * _size + cursorX}")
      repaint()

  init

  private def init : Unit =
    setFocusable(true)
    setLayout(new BorderLayout())
    val northPanel = new JPanel()
    northPanel.add(coordLabel)
    add("North",northPanel)
    add("Center",new Canvas)
}
