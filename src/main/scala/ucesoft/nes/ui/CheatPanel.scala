package ucesoft.nes.ui

import ucesoft.nes.misc.Preferences
import ucesoft.nes.util.Cheat

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout}
import javax.swing.event.{DocumentEvent, DocumentListener, ListSelectionEvent, ListSelectionListener}
import javax.swing.{BorderFactory, Box, BoxLayout, DefaultListModel, JButton, JDialog, JFrame, JLabel, JList, JPanel, JScrollPane, JTable, JTextField, SwingConstants}
import javax.swing.table.AbstractTableModel

class CheatPanel(frame:JFrame,manager:Cheat.CheatManager,gameName:Option[String],pref:Preferences) extends JPanel:
  private var applied = false
  private val sortedCheats = Cheat.cheatsDB.toList.sortBy(_._1)
  private class TableModel extends AbstractTableModel:
    case class Row(game:String,codes:String,action:String)
    private var rows : Array[Row] = applyFilter(gameName.getOrElse(""))

    override def getColumnName(column: Int): String =
      column match
        case 0 => "Game name"
        case 1 => "Cheat codes"
        case 2 => "Action"

    override def getRowCount: Int = rows.length
    override def getColumnCount: Int = 3
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 0 => rows(rowIndex).game
        case 1 => rows(rowIndex).codes
        case 2 => rows(rowIndex).action

    private def applyFilter(nameFilter:String): Array[Row] =
      val filter = nameFilter.toUpperCase()
      (for kv <- sortedCheats
           codes <- kv._2.codes
           if kv._1.toUpperCase().contains(filter) yield
        Row(kv._2.game,codes.codes.mkString(","),codes.effect)
      ).toArray

    def setFilter(filter:String): Unit =
      rows = applyFilter(filter)
      fireTableDataChanged()

  init()

  val dialog : JDialog = {
    val dialog = new JDialog(frame,"Cheats",true)
    dialog.setLocationRelativeTo(frame)
    dialog.getContentPane.add("Center",this)
    dialog.pack()
    dialog.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit =
        dialog.dispose()
    })
    dialog
  }

  def isApplied : Boolean = applied

  private def init(): Unit =
    val applyButton = new JButton("Apply changes")
    applyButton.setEnabled(false)

    setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS))
    val filterPanel = new JPanel(new BorderLayout())
    val filterText = new JTextField(50)
    filterText.setText(gameName.getOrElse(""))
    var dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummyPanel.add(new JLabel("Name filter:",SwingConstants.RIGHT))
    dummyPanel.add(filterText)
    filterPanel.add("North",dummyPanel)
    val tableModel = new TableModel
    val filterTable = new JTable(tableModel)
    val tableSp = new JScrollPane(filterTable)
    filterPanel.add("Center",tableSp)
    filterPanel.setBorder(BorderFactory.createTitledBorder("Cheat Database"))

    filterText.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = tableModel.setFilter(filterText.getText)
      override def removeUpdate(e: DocumentEvent): Unit = tableModel.setFilter(filterText.getText)
      override def changedUpdate(e: DocumentEvent): Unit = tableModel.setFilter(filterText.getText)
    })

    val filterButtonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val addSelectedButton = new JButton("Add selected cheats")
    addSelectedButton.setEnabled(false)
    filterTable.getSelectionModel.addListSelectionListener(_ => addSelectedButton.setEnabled(filterTable.getSelectedRow > 0) )
    val cheatCodeText = new JTextField(8)
    val addCheatCodeButton = new JButton("Add cheat code")
    addCheatCodeButton.setEnabled(false)
    cheatCodeText.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = checkCheat()
      override def removeUpdate(e: DocumentEvent): Unit = checkCheat()
      override def changedUpdate(e: DocumentEvent): Unit = checkCheat()
      private def checkCheat(): Unit =
        val codeLen = cheatCodeText.getText.length
        if codeLen == 6 || codeLen == 8 then
          Cheat.gameGenieDecode(cheatCodeText.getText.toUpperCase()) match
            case Some(Cheat.Cheat(address,data,compare,_)) =>
              cheatCodeText.setToolTipText(s"address = ${address.toHexString.toUpperCase()} data = ${data.toHexString.toUpperCase()} ${if compare.isDefined then s" compare value = ${compare.get.toHexString.toUpperCase()}" else ""}")
              addCheatCodeButton.setEnabled(true)
            case None =>
        else
          cheatCodeText.setToolTipText("")
          addCheatCodeButton.setEnabled(false)
    })
    filterButtonPanel.add(addSelectedButton)
    filterButtonPanel.add(new JLabel("Cheat code:",SwingConstants.RIGHT))
    filterButtonPanel.add(cheatCodeText)
    filterButtonPanel.add(addCheatCodeButton)
    filterPanel.add("South",filterButtonPanel)

    add(filterPanel)

    add(Box.createRigidArea(new Dimension(0,10)))
    // ========================================================================

    val listModel = new DefaultListModel[String]
    addCheatCodeButton.addActionListener(_ => {
      if !listModel.contains(cheatCodeText.getText.toUpperCase()) then
        listModel.addElement(cheatCodeText.getText.toUpperCase())
        applyButton.setEnabled(true)
    })
    addSelectedButton.addActionListener(_ => {
      for r <- filterTable.getSelectedRows do
        val codes = tableModel.getValueAt(r,1).toString.toUpperCase().split(",")
        for code <- codes do
          if !listModel.contains(code) then
            listModel.addElement(code)
            applyButton.setEnabled(true)
    })
    for c <- manager.getCheats() do listModel.addElement(c.code)
    val cheatList = new JList(listModel)
    cheatList.setPreferredSize(new Dimension(200,0))

    val cheatsPanel = new JPanel(new BorderLayout())
    cheatsPanel.setBorder(BorderFactory.createTitledBorder("Cheats"))
    val listSp = new JScrollPane(cheatList)
    dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummyPanel.add(listSp)
    val removeButton = new JButton("Remove selected")
    removeButton.setEnabled(false)
    removeButton.addActionListener(_ => {
      import scala.jdk.CollectionConverters.*
      for r <- cheatList.getSelectedValuesList.asScala do
        listModel.removeElement(r)
        applyButton.setEnabled(true)
    })
    dummyPanel.add(removeButton)
    cheatList.addListSelectionListener(_ => removeButton.setEnabled(cheatList.getSelectionModel.getSelectedItemsCount > 0))
    cheatsPanel.add("West",dummyPanel)

    add(cheatsPanel)
    // ========================================================================
    dummyPanel = new JPanel(new FlowLayout())
    dummyPanel.add(applyButton)
    applyButton.addActionListener(_ => {
      applyButton.setEnabled(false)
      manager.removeAllCheats()
      applied = true
      val sb = new StringBuilder
      for r <- 0 until listModel.getSize do
        Cheat.gameGenieDecode(listModel.elementAt(r)) match
          case Some(cheat) =>
            manager.addCheat(cheat)
            if r > 0 then sb.append('+')
            sb.append(cheat.code)
          case None =>
      pref.updateWithoutNotify(Preferences.CHEATS,sb.toString)
    })

    add(dummyPanel)