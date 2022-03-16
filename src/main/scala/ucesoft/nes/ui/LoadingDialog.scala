package ucesoft.nes.ui

import java.awt.{Dimension, Window}
import javax.swing.{JDialog, JPanel, JProgressBar}

class LoadingDialog(win:Window,message:String) extends JDialog(win,message):

  init()

  private def init(): Unit =
    val progress = new JProgressBar()
    progress.setIndeterminate(true)
    progress.setPreferredSize(new Dimension(300,15))
    val panel = new JPanel()
    panel.add(progress)
    getContentPane.add("Center",panel)
    pack()
    setLocationRelativeTo(win)
