package ucesoft.nes.ui

import ucesoft.nes.{PPU, Palette}
import ucesoft.nes.util.ImageScaler

import java.awt.event.{WindowAdapter, WindowEvent, WindowFocusListener}
import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Graphics, Toolkit}
import java.awt.image.MemoryImageSource
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{BorderFactory, JCheckBox, JComboBox, JComponent, JDialog, JFrame, JLabel, JPanel, JSpinner, SpinnerNumberModel}

class TileViewer(frame:JFrame,ppu:PPU,closeAction:() => Unit,focusListener:() => Unit) extends JComponent with Runnable:
  private val leftRight : Array[TileComponent] = Array(null,null)
  private var palette = 0
  private val paletteBox : Array[PaletteColor] = Array.fill[PaletteColor](32)(new PaletteColor(Color.BLACK))
  private val thread = new Thread(this,"TileViewer")
  private val lock = new Object
  private var bank = -1

  // CONSTRUCTOR ==============
  init()
  // ==========================

  val dialog : JDialog = {
    val dialog = new JDialog(frame,"Tile viewer")
    dialog.getContentPane.add("Center",this)
    dialog.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = closeAction()
    })
    dialog.addWindowFocusListener(new WindowFocusListener {
      override def windowGainedFocus(e: WindowEvent): Unit =
        focusListener()
      override def windowLostFocus(e: WindowEvent): Unit = {}
    })
    dialog.pack()
    dialog
  }

  class PaletteColor(var color:Color) extends JComponent {
    override def paint(g: Graphics): Unit =
      val size = getSize()
      g.setColor(color)
      g.fillRect(0,0,size.width - 1,size.height - 1)
  }

  override def run(): Unit =
    while true do
      lock.synchronized {
        lock.wait()
        _updateTiles()
      }

  def updateTiles(): Unit = lock.synchronized {
    lock.notify()
  }

  def _updateTiles(): Unit =
    for(t <- 0 to 1) do
      val tiles = ppu.getPatternTableImage(t,palette,if bank == -1 then None else Some(bank))
      val image = new MemoryImageSource(tiles.width, tiles.height, tiles.pixels, 0, tiles.width)
      val img = Toolkit.getDefaultToolkit.createImage(image)
      val doubleImage = ImageScaler.resizeImage(img,tiles.width << 1,tiles.height << 1)
      if leftRight(t) == null then
        leftRight(t) = new TileComponent(doubleImage,16)
      else
        leftRight(t).image = doubleImage
        leftRight(t).repaint()

    val colors = ppu.getPalette.palette
    for(i <- 0 until 32) do
      val pal = ppu.palette(i)
      paletteBox(i).color = new Color(colors(0)(pal))
      paletteBox(i).setToolTipText(s"0x${pal.toHexString.toUpperCase()}")
      paletteBox(i).repaint()

  private def init(): Unit =
    setLayout(new BorderLayout())
    _updateTiles()
    val tilePanel = new JPanel()
    tilePanel.setBorder(BorderFactory.createTitledBorder("Tiles"))
    tilePanel.add(leftRight(0))
    tilePanel.add(leftRight(1))

    for(p <- paletteBox) p.setPreferredSize(new Dimension(20,20))

    val palettePanel = new JPanel(new BorderLayout())
    val up = new JPanel()
    val down = new JPanel()
    for(i <- 0 to 15) up.add(paletteBox(i))
    for(i <- 16 to 31) down.add(paletteBox(i))
    palettePanel.add("Center",up)
    palettePanel.add("South",down)
    palettePanel.setBorder(BorderFactory.createTitledBorder("Colors"))

    val paletteCombo = new JComboBox[String](Array("0","1","2","3"))
    paletteCombo.setEditable(false)
    val comboPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    comboPanel.add(new JLabel("Show with palette index:"))
    comboPanel.add(paletteCombo)
    paletteCombo.addActionListener(_ => {
      palette = paletteCombo.getSelectedIndex
      _updateTiles()
    })
    val bankCheckbox = new JCheckBox("Look into CHR banks")
    val bankSpinning = new JSpinner(new SpinnerNumberModel(0,0,ppu.getCartridge().chrBankSize - 1,1))
    bankSpinning.getModel.addChangeListener(_ => bank = bankSpinning.getModel.getValue.asInstanceOf[Int] )
    bankCheckbox.addActionListener(_ => {
      bankSpinning.setEnabled(bankCheckbox.isSelected)
      if !bankCheckbox.isSelected then bank = -1
      else bank = bankSpinning.getModel.getValue.asInstanceOf[Int]
    })
    bankSpinning.setEnabled(false)
    comboPanel.add(bankCheckbox)
    comboPanel.add(bankSpinning)

    add("North",comboPanel)
    add("Center",tilePanel)
    add("South",palettePanel)

    thread.start()
