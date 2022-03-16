package ucesoft.nes.ui

import ucesoft.nes.util.CartDB
import ucesoft.nes.util.CartDB.VSGame

import java.awt.{BorderLayout, GridLayout}
import javax.swing.{BorderFactory, JButton, JComboBox, JDialog, JFrame, JLabel, JPanel}

object VsDIPSwitchPanel:
  def getDialog(frame:JFrame,title:String,switches:List[CartDB.VSSwitches],applyAction:List[CartDB.VSSwitches] => Unit): JDialog =
    val d = new JDialog(frame,title,true)
    d.getContentPane.add("Center",new VsDIPSwitchPanel(switches,() => d.dispose(),applyAction))
    d.pack()
    d.setLocationRelativeTo(frame)
    d

class VsDIPSwitchPanel(switches:List[CartDB.VSSwitches],disposeAction: () => Unit,applyAction:List[CartDB.VSSwitches] => Unit) extends JPanel:
  init()

  private def init(): Unit =
    val dummyPanel = new JPanel(new GridLayout(switches.length,2))
    dummyPanel.setBorder(BorderFactory.createTitledBorder("DIP switches"))
    setLayout(new BorderLayout())
    add("Center",dummyPanel)
    val combos = (for dip <- switches yield
      val choices = new JComboBox[String](dip.switches.map(_.label))
      choices.setSelectedIndex(dip.activeIndex)
      dummyPanel.add(new JLabel(dip.label))
      dummyPanel.add(choices)
      choices).toArray

    val buttonPanel = new JPanel()
    val cancelButton = new JButton("Cancel")
    cancelButton.addActionListener(_ => disposeAction())
    buttonPanel.add(cancelButton)
    val applyButton = new JButton("Apply")
    applyButton.addActionListener(_ => {
      disposeAction()
      val newDips = switches.zipWithIndex map { d => d._1.copy(activeIndex = combos(d._2).getSelectedIndex)}
      applyAction(newDips)
    })
    buttonPanel.add(applyButton)
    add("South",buttonPanel)

