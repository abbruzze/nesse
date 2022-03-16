package ucesoft.nes.ui.control

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.nes.controller.FamilyKeyboard

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.{Color, Component, FlowLayout, Font, Insets, Rectangle}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.{JButton, JDialog, JFileChooser, JFrame, JLabel, JPanel, UIManager}
import scala.collection.mutable

class KeyboardEditor(frame:JFrame,layout:FamilyKeyboard.KeyboardLayout,applyAction: FamilyKeyboard.KeyboardLayout => Unit) extends JPanel:
  val dialog : JDialog = new JDialog(frame,"Family Keyboard Editor",true)
  private val applyLayout = new JButton("Apply layout")

  private val newLayout = {
    val map = new mutable.HashMap[Int,FamilyKeyboard.Key]
    map.addAll(layout)
    map
  }

  private def mkb(s:String,key:FamilyKeyboard.Key,bg:Color = Color.GRAY): JButton =
    val b = new JButton(s)
    b.setBackground(bg)
    b.setForeground(Color.BLACK)
    b.setMargin(new Insets(0, 0, 0, 0))
    b.addActionListener(_ => actionListener(key))
    b

  private def actionListener(key:FamilyKeyboard.Key): Unit =
    val keyDialog = new JDialog(dialog,s"Press a key for $key",true)
    val cancel = new JButton("Cancel")
    cancel.addActionListener(_ => keyDialog.dispose())
    var dummy = new JPanel
    dummy.add(cancel)
    keyDialog.getContentPane.add("South",dummy)
    dummy = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val code = newLayout.find(_._2 == key).map(_._1).getOrElse(-1)
    dummy.add(new JLabel(if code == -1 then "Not bound" else s"Actual binding: ${KeyEvent.getKeyText(code)}"))
    keyDialog.getContentPane.add("North",dummy)
    dummy = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummy.add(new JLabel(s"Press a key for $key"))
    keyDialog.getContentPane.add("Center",dummy)
    keyDialog.pack()
    keyDialog.addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit =
        if code != -1 then
          newLayout -= code
        newLayout.update(e.getExtendedKeyCode,key)
        applyLayout.setEnabled(true)
        keyDialog.dispose()
    })
    keyDialog.setLocationRelativeTo(dialog)
    keyDialog.setFocusable(true)
    keyDialog.requestFocus()
    keyDialog.setVisible(true)

  private def saveAs(): Unit =
    val fc = new JFileChooser
    fc.showSaveDialog(dialog) match {
      case JFileChooser.APPROVE_OPTION =>
        FamilyKeyboard.saveLayout(fc.getSelectedFile.toString,newLayout.toMap)
      case _ =>
    }

  init()

  private def init(): Unit =
    dialog.setLocationRelativeTo(frame)
    setLayout(null)
    import FamilyKeyboard.Key
    import Key.*

    val XBORDER = 20
    val YBORDER = 40
    val CBORDER = 5
    var x = XBORDER
    var y = YBORDER
    val HEIGHT = 20

    // 1st row F1 - F8
    for i <- 1 to 8 do
      val b = mkb(s"F$i",Key.valueOf(s"F$i"),Color.RED)
      add(b)
      b.setBounds(x,y,50,HEIGHT)
      x += CBORDER + 50
    // 2nd row
    y += CBORDER + HEIGHT
    x = XBORDER + 10
    for i <- 0 to 9 do
      val index = (i + 1) % 10
      val b = mkb(s"$index",Key.valueOf(s"_$index"))
      add(b)
      b.setBounds(x,y,23,HEIGHT)
      x += CBORDER + 23
    for i <- Array(("-",MINUS),("^",POWER),("\\",UNKNOWN),("stp",STOP)) do
      val b = mkb(s"${i._1}",i._2)
      add(b)
      b.setBounds(x,y,23,HEIGHT)
      x += CBORDER + 23
    // 3rd row
    y += CBORDER + HEIGHT
    x = XBORDER
    for i <- Array("esc") ++ "QWERTYUIOP@[".toCharArray.map(_.toString) do
      val key = i match
        case "@" => AT
        case "[" => OPEN_RECT
        case x => Key.valueOf(x.toUpperCase())

      val b = mkb(s"$i",key)
      add(b)
      b.setBounds(x,y,23,HEIGHT)
      x += CBORDER + 23
    val ret = mkb("RETURN",RETURN,Color.RED)
    add(ret)
    ret.setBounds(x,y,60,HEIGHT)
    // 4th row
    y += CBORDER + HEIGHT
    x = XBORDER + 10
    for i <- Array("ctr") ++ "ASDFGHJKL;:]".toCharArray.map(_.toString) ++ Array("kn") do
      val key = i match
        case "ctr" => CTR
        case ";" => SEMICOLON
        case ":" => COLON
        case "]" => CLOSED_RECT
        case "kn" => KANA
        case x => Key.valueOf(x.toUpperCase())
      val b = mkb(s"$i",key)
      add(b)
      b.setBounds(x,y,23,HEIGHT)
      x += CBORDER + 23
    // 5th row
    y += CBORDER + HEIGHT
    x = XBORDER
    val lshift = mkb("SHIFT",LSHIFT,Color.RED)
    lshift.setEnabled(false)
    add(lshift)
    lshift.setBounds(x,y,50,HEIGHT)
    x += CBORDER + 50
    for i <- "ZXCVBNM,./_".toCharArray.map(_.toString) do
      val key = i match
        case "," => COMMA
        case "." => PERIOD
        case "/" => SLASH
        case "_" => UNDERSCORE
        case x => Key.valueOf(x.toUpperCase())
      val b = mkb(s"$i",key)
      add(b)
      b.setBounds(x,y,23,HEIGHT)
      x += CBORDER + 23
    val rshift = mkb("SHIFT",RSHIFT,Color.RED)
    rshift.setEnabled(false)
    add(rshift)
    rshift.setBounds(x,y,50,HEIGHT)
    // 6th row
    y += CBORDER + HEIGHT
    x = XBORDER + 84
    val grp = mkb("gr",GRPH)
    add(grp)
    grp.setBounds(x,y,23,HEIGHT)
    x += CBORDER + 23
    val space = mkb("SPACE",SPACE,Color.RED)
    add(space)
    space.setBounds(x,y,220,HEIGHT)
    // right zone
    y = CBORDER + 80
    x = XBORDER + 430
    for i <- Array("clr","ins","del") do
      val b = mkb(s"$i",Key.valueOf(i.toUpperCase()))
      add(b)
      b.setBounds(x,y,23,HEIGHT)
      x += CBORDER + 23

    y += CBORDER + HEIGHT
    val up = mkb(s"/\\",UP,Color.RED)
    add(up)
    x = XBORDER + 450
    up.setBounds(x,y,40,HEIGHT)
    y += CBORDER + HEIGHT
    x = XBORDER + 430
    for i <- Array("<",">") do
      val b = mkb(s"$i",if i == "<" then LEFT else RIGHT,Color.RED)
      add(b)
      b.setBounds(x,y,40,HEIGHT)
      x += CBORDER + 40

    y += CBORDER + HEIGHT
    val down = mkb(s"\\/",DOWN,Color.RED)
    add(down)
    x = XBORDER + 450
    down.setBounds(x,y,40,HEIGHT)

    applyLayout.setEnabled(false)
    applyLayout.addActionListener(_ => {
      applyAction(newLayout.toMap)
      applyLayout.setEnabled(false)
    } )
    dialog.getContentPane.add("Center",this)
    val south = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val saveAsButton = new JButton("Save as ...")
    saveAsButton.addActionListener(_ => saveAs() )
    south.add(saveAsButton)
    south.add(applyLayout)
    dialog.getContentPane.add("South",south)
    dialog.setSize(600,300)
    dialog.setIconImage(ImageIO.read(this.getClass().getResourceAsStream("/resources/images/controller.png")))