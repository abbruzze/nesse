package ucesoft.nes.ui.control


import ucesoft.nes.NES
import ucesoft.nes.misc.Preferences

import java.awt.{BorderLayout, CardLayout, Color, FlowLayout}
import javax.swing.*
import javax.swing.event.{TreeExpansionEvent, TreeSelectionEvent, TreeSelectionListener, TreeWillExpandListener}
import javax.swing.tree.*
import scala.annotation.tailrec

object ControlPanel {
  def getDialog(parent:JFrame,nes:NES,videoControl:VideoControl) : JDialog = {
    val dialog = new JDialog(parent,"Control panel")

    dialog.getContentPane.add("Center",new ControlPanel(parent,nes,videoControl))
    dialog.pack()
    dialog.setResizable(false)
    dialog.setLocationRelativeTo(parent)
    dialog
  }
}

class ControlPanel(frame:JFrame,nes:NES,videoControl:VideoControl) extends JPanel with TreeSelectionListener {
  private val cardPanel = new JPanel(new CardLayout())
  private val root = new DefaultMutableTreeNode("Preferences")
  private val tree = new JTree(root)

  init

  @tailrec private def expandTree(tree:JTree) : Unit = {
    val rowCount = tree.getRowCount
    for(r <- 0 to rowCount) tree.expandRow(r)
    if (tree.getRowCount != rowCount) expandTree(tree)
  }

  private def init : Unit = {
    tree.addTreeWillExpandListener(new TreeWillExpandListener {
      override def treeWillExpand(event: TreeExpansionEvent): Unit = {}
      override def treeWillCollapse(event: TreeExpansionEvent): Unit = throw new ExpandVetoException(event)
    })
    tree.setShowsRootHandles(false)
    tree.getSelectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
    tree.getSelectionModel.addTreeSelectionListener(this)
    val renderer = tree.getCellRenderer.asInstanceOf[DefaultTreeCellRenderer]
    renderer.setLeafIcon(null)
    renderer.setClosedIcon(null)
    renderer.setOpenIcon(null)
    // general
    val general = new DefaultMutableTreeNode("General")
    // general -> model
    val model = new DefaultMutableTreeNode("Model")
    general.add(model)
    root.add(general)
    // joystick
    val joy = new DefaultMutableTreeNode("Joystick")
    root.add(joy)
    // keyboard
    val kb = new DefaultMutableTreeNode("Family Keyboard")
    root.add(kb)
    // Video
    val video = new DefaultMutableTreeNode("Video")
    root.add(video)
    // Peripherals
    val fds = new DefaultMutableTreeNode("FDS")
    root.add(fds)

    val treeView = new JScrollPane(tree)

    expandTree(tree)
    tree.setSelectionRow(2)

    setLayout(new BorderLayout())
    add("West",treeView)
    add("Center",cardPanel)

    val save = new JButton("Save configuration")
    save.addActionListener(_ => {
      nes.savePreferences()
      JOptionPane.showMessageDialog(this,"Configuration saved", "Configuration",JOptionPane.INFORMATION_MESSAGE)
    })

    val buttonPanel = new JPanel(new BorderLayout())
    val dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummyPanel.add(save)
    val autosave = new JCheckBox("Auto save preferences on exit")
    autosave.setSelected(nes.preferences.get[Boolean](Preferences.AUTOSAVE_PREFERENCES).map(_.value).getOrElse(false))
    autosave.addActionListener(_ => nes.preferences.update[Boolean](Preferences.AUTOSAVE_PREFERENCES,autosave.isSelected))
    dummyPanel.add(autosave)

    buttonPanel.add("North",dummyPanel)
    add("South",buttonPanel)

    val southPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    southPanel.setBackground(Color.black)
    southPanel.add(new JLabel(new ImageIcon(getClass.getResource("/resources/images/controller.png"))))
    buttonPanel.add("South",southPanel)

    cardPanel.add(new JoystickMasterPanel(nes),"Joystick")
    cardPanel.add(new KeyboardPanel(frame,nes),"Family Keyboard")
    cardPanel.add(new FDSPanel(nes),"FDS")
    cardPanel.add(new VideoPanel(nes),"Video")
    cardPanel.add(new GeneralPanel(nes),"Model")

    cardPanel.getLayout.asInstanceOf[CardLayout].show(cardPanel,"Model")
  }

  override def valueChanged(e: TreeSelectionEvent): Unit = {
    val node = tree.getLastSelectedPathComponent.asInstanceOf[DefaultMutableTreeNode]
    if (node.isLeaf) {
      val card = node.getUserObject.toString
      cardPanel.getLayout.asInstanceOf[CardLayout].show(cardPanel,card)
    }
  }
}

