package ucesoft.nes.ui

import ucesoft.nes.PPU

import java.awt.event.{WindowAdapter, WindowEvent, WindowFocusListener}
import java.awt.image.MemoryImageSource
import java.awt.*
import javax.swing.*

class NameTableViewer(frame:JFrame,ppu:PPU,closeAction:() => Unit,focusListener:() => Unit) extends JComponent with Runnable:
  private val tables = Array.fill[JLabel](4)(new JLabel())
  private var showScroll = true
  private val thread = new Thread(this,"NameTableViewer")
  private val lock = new Object

  // CONSTRUCTOR ==============
  init()
  // ==========================

  val dialog : JDialog = {
    val dialog = new JDialog(frame,"Name table viewer")
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

  private def makeNT(i:Int): Image =
    val ntv = ppu.getNameTableImage(i,showScroll)
    val image = new MemoryImageSource(ntv.width, ntv.height, ntv.pixels, 0, ntv.width)
    Toolkit.getDefaultToolkit.createImage(image)

  override def run(): Unit =
    while true do
      lock.synchronized {
        lock.wait()
        _updateNameTables()
      }

  def updateNameTables(): Unit = lock.synchronized {
    lock.notify()
  }

  private def _updateNameTables(): Unit =
    import ucesoft.nes.Cartridge.Mirroring.*

    ppu.getCartridge().getMirroring() match
      case VERTICAL =>
        tables(0).setIcon(new ImageIcon(makeNT(0)))
        tables(1).setIcon(new ImageIcon(makeNT(1)))
        tables(2).setIcon(tables(0).getIcon)
        tables(3).setIcon(tables(1).getIcon)
      case HORIZONTAL =>
        tables(0).setIcon(new ImageIcon(makeNT(0)))
        tables(1).setIcon(tables(0).getIcon)
        tables(2).setIcon(new ImageIcon(makeNT(2)))
        tables(3).setIcon(tables(2).getIcon)
      case FOUR_SCREEN =>
        tables(0).setIcon(new ImageIcon(makeNT(0)))
        tables(1).setIcon(new ImageIcon(makeNT(1)))
        tables(2).setIcon(new ImageIcon(makeNT(2)))
        tables(3).setIcon(new ImageIcon(makeNT(3)))
      case ALL_MIRROR0 =>
        tables(0).setIcon(new ImageIcon(makeNT(0)))
        tables(1).setIcon(tables(0).getIcon)
        tables(2).setIcon(tables(0).getIcon)
        tables(3).setIcon(tables(0).getIcon)
      case ALL_MIRROR1 =>
        tables(1).setIcon(new ImageIcon(makeNT(1)))
        tables(0).setIcon(tables(1).getIcon)
        tables(2).setIcon(tables(1).getIcon)
        tables(3).setIcon(tables(1).getIcon)
        
  private def init(): Unit =
    setLayout(new BorderLayout())
    _updateNameTables()
    val tablePanel = new JPanel(new GridLayout(2,2))
    for(t <- tables) do
      tablePanel.add(t)

    val scrollPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val scrollCheckBox = new JCheckBox("Show scroll lines")
    scrollCheckBox.setSelected(true)
    scrollPanel.add(scrollCheckBox)
    scrollCheckBox.addActionListener(_ => showScroll = scrollCheckBox.isSelected )

    add("North",scrollPanel)
    add("Center",tablePanel)

    thread.start()