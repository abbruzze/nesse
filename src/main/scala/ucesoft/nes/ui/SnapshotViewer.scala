package ucesoft.nes.ui

import ucesoft.nes.util.{ImageScaler, StateSnapShotManager}

import java.awt.event.{MouseAdapter, MouseEvent, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Color, Dimension, FlowLayout}
import javax.swing.{BorderFactory, ImageIcon, JButton, JComponent, JDialog, JFrame, JLabel, JPanel, JScrollPane, ScrollPaneConstants}

object SnapshotViewer:
  def getSnapshotViewerDialog(frame:JFrame,manager:StateSnapShotManager,selectionHandler: StateSnapShotManager.Snap => Unit) : JDialog =
    val dialog = new JDialog(frame,"Snapshot viewer",true)
    dialog.getContentPane.add(new SnapshotViewer(manager,selectionHandler,dialog))
    dialog.pack()
    dialog.setLocationRelativeTo(frame)
    dialog.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit =
        dialog.dispose()
    })
    dialog

class SnapshotViewer(manager:StateSnapShotManager,selectionHandler: StateSnapShotManager.Snap => Unit,dialog:JDialog) extends JComponent with SwingAware:
  private var lastSelected : SnapImage = _
  private val restoreButton = new JButton("Restore snapshot")

  private class SnapImage(val snap:StateSnapShotManager.Snap,id:Int) extends JComponent:
    private var snapSelected = false

    setLayout(new BorderLayout())
    val image = ImageScaler.resizeImage(snap.image,snap.image.getWidth(null) / 2,snap.image.getHeight(null) / 2)
    add("Center",new JLabel(new ImageIcon(image)))
    val southPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
    southPanel.add(new JLabel(new java.util.Date(snap.ts).toString))
    add("South",southPanel)
    val northPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
    northPanel.add(new JLabel(id.toString))
    add("North",northPanel)
    addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit =
        if lastSelected != null then lastSelected.selectSnap(false)
        lastSelected = SnapImage.this
        selectSnap(true)
        restoreButton.setEnabled(true)

      override def mouseEntered(e: MouseEvent): Unit = setBorder(BorderFactory.createRaisedSoftBevelBorder())
      override def mouseExited(e: MouseEvent): Unit = selectSnap(snapSelected)
    })

    def selectSnap(selected:Boolean): Unit =
      snapSelected = selected
      if selected then setBorder(BorderFactory.createLineBorder(Color.BLACK,2)) else setBorder(BorderFactory.createEmptyBorder())



  init()

  private def init(): Unit =
    val listPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val sp = new JScrollPane(listPanel)
    sp.getHorizontalScrollBar.setUnitIncrement(256)
    sp.getHorizontalScrollBar.setBlockIncrement(512)
    setLayout(new BorderLayout())
    add("Center",sp)
    setPreferredSize(new Dimension(600,400))

    val snaps = manager.lastSnapList()
    for s <- snaps.zipWithIndex do
      swing {
        manager.getSnap(s._1) match
          case Some(snap) =>
            listPanel.add(new SnapImage(snap, s._2 + 1))
          case None =>
      }

    val southPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    southPanel.add(new JLabel("Select a snapshot to restore"))
    southPanel.add(restoreButton)
    restoreButton.setEnabled(false)
    restoreButton.addActionListener(_ => {
      selectionHandler(lastSelected.snap)
      dialog.dispose()
    })
    add("South",southPanel)