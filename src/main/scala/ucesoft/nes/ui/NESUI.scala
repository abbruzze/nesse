package ucesoft.nes.ui

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.nes.*
import ucesoft.nes.controller.*
import ucesoft.nes.cpu.CPU65xx.CPUJammedException
import ucesoft.nes.mappers.MapperFactory
import ucesoft.nes.mappers.fds.FDS
import ucesoft.nes.misc.{FullScreenMode, Preferences}
import ucesoft.nes.trace.{InspectPanel, InspectPanelDialog, TraceDialog}
import ucesoft.nes.ui.control.{ControlPanel, VideoControl}
import ucesoft.nes.util.CartDB.VSSwitches
import ucesoft.nes.util.{Cheat, ImageScaler, StateSnapShotManager}

import java.awt.{Cursor, Dimension, RenderingHints}
import java.awt.event.{WindowAdapter, WindowEvent, WindowFocusListener}
import java.awt.image.MemoryImageSource
import java.io.*
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import javax.imageio.ImageIO
import javax.swing.*
import javax.swing.filechooser.FileFilter
import scala.collection.mutable.ListBuffer

object NESUI:
  def main(args: Array[String]): Unit =
    val nes = new NESUI
    try
      nes.boot()
      nes.configure(args)
      nes.run()
    catch
      case i:Preferences.PreferenceIllegalArgumentException =>
        println(s"Bad command line argument: ${i.getMessage}")
        sys.exit(100)
      case t:Throwable =>
        nes.errorHandler(t)
        sys.exit(1)

class NESUI extends SwingAware with VideoControl:
  private[this] val nes = new NES(errorHandler)
  private[this] var frame : JFrame = _
  private[this] var display : Display = _
  private[this] var traceDialog : TraceDialog = _
  private[this] var inspectDialog : InspectPanelDialog = _
  private[this] var lastDirectory : String = _
  private[this] var headless = false
  private[this] var systemInitialized = false
  private[this] var lastSavedStateFile = ""

  private[this] var stateSnapShotManager : StateSnapShotManager = _

  protected var traceItem : JCheckBoxMenuItem = _
  protected var inspectItem : JCheckBoxMenuItem = _
  protected var warpModeItem : JCheckBoxMenuItem = _
  protected var overwriteLastSavedStateItem : JMenuItem = _
  protected var reloadLastSavedStateItem : JMenuItem = _
  protected var snapshotMode : JCheckBoxMenuItem = _
  protected var nameTableViewerItem : JMenuItem = _
  protected var tileViewerItem : JMenuItem = _
  protected var deviceMenuItem : JMenu = _
  protected var diskMenuItem : JMenu = _
  protected var changeDiskItem,ejectDiskItem : JMenuItem = _
  protected var cheatsItem : JMenuItem = _
  protected var vsMenu : JMenu = _

  protected var cheatsFromCommandLine : List[Cheat.Cheat] = Nil

  protected var controlPanelDialog : JDialog = _

  protected var vsSwitches : List[VSSwitches] = Nil

  protected val frameListeners = new collection.mutable.HashMap[String,() => Unit]
  protected var tileViewer : TileViewer = _
  protected var nameTableViewer : NameTableViewer = _

  protected var tvMode : Option[Cartridge.TV] = None
  protected var forceZapperOnPort: Option[Int] = None
  protected var forceKeyboard = false
  protected var paused = false

  protected def closeFrameListener(id:String): Unit =
    frameListeners -= id
    if frameListeners.size == 0 then nes.ppu.setFrameListener(null)

  def boot() : Unit =
    if System.getProperty("swing.defaultlaf") == null then
      FlatLightLaf.setup()
      JFrame.setDefaultLookAndFeelDecorated(false)
      JDialog.setDefaultLookAndFeelDecorated(false)
      UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")

    frame = new JFrame(s"NESSE v${Version.VERSION}")
    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = shutdown()
    })
    frame.addWindowFocusListener(new WindowFocusListener {
      override def windowGainedFocus(e: WindowEvent): Unit =
        if !paused then
          play()

      override def windowLostFocus(e: WindowEvent): Unit =
        if nes.preferences.get[Boolean](Preferences.PAUSE_IF_LOST_FOCUS).map(_.value).getOrElse(true) && !traceDialog.isVisible && frame.isVisible() then
          pause(true)
    })
    frame.setIconImage(ImageIO.read(getClass.getResourceAsStream("/resources/images/controller.png")))

    display = new Display(PPU.DISPLAY_WIDTH,PPU.DISPLAY_HEIGHT,frame.getTitle,frame,nes.clk)
    frame.getContentPane.add("Center",display)
    nes.add(display)
    nes.ppu.setDisplay(display)
    stateSnapShotManager = new StateSnapShotManager(nes,display)
    
    frame.addKeyListener(nes.inputManager)
    display.addMouseListener(nes.inputManager)
    display.addMouseMotionListener(nes.inputManager)
    display.setDisplaySizeChangedListener(nes.inputManager)

    // Trace
    traceDialog = TraceDialog.getTraceDialog("Trace",frame,nes.cpuMem,nes.cpu,display,nes.ppu,nes.getCartridge().orNull, () => {
      traceItem.setSelected(false)
      traceDialog.setVisible(false)
    })
    //traceDialog.forceTracing(true)

    Log.setOutput(traceDialog.logPanel.writer)

    // Frame
    buildMenu()

    // Display zoom
    zoomDisplay(2)

    // DND
    frame.setTransferHandler(new DNDHandler(handleDND _))

  end boot

  protected def handleDND(file:File) : Unit =
    val fileName = file.getName.toUpperCase()
    if fileName.endsWith(".FDS") then
      loadFDS(Some(file.toString))
    else
      loadCart(Some(file.toString))


  protected def shutdown() : Unit =
    nes.getCartridge().foreach(_.eject())
    // SAVE CONFIGURATION
    val pos = frame.getLocationOnScreen()
    nes.configuration.setProperty(Preferences.XY,s"${pos.x},${pos.y}")
    if (nes.preferences.get[Boolean](Preferences.AUTOSAVE_PREFERENCES).map(_.value).getOrElse(false)) nes.savePreferences()
    else nes.saveConfiguration()

    sys.exit(0)


  protected def errorHandler(t:Throwable) : Unit =
    t match
      case j:CPUJammedException =>
        JOptionPane.showConfirmDialog(frame,
          s"CPU[${j.cpuID}] jammed at " + Integer.toHexString(j.pcError) + ". Do you want to open debugger (yes), reset (no) or continue (cancel) ?",
          "CPU jammed",
          JOptionPane.YES_NO_CANCEL_OPTION,
          JOptionPane.ERROR_MESSAGE) match {
          case JOptionPane.YES_OPTION =>
            if (traceDialog != null) traceDialog.forceTracing(true)
            Log.setOutput(traceDialog.logPanel.writer)
            traceDialog.setVisible(true)
            traceItem.setSelected(true)
          case JOptionPane.CANCEL_OPTION => // continue
          case _ =>
            reset(true)
        }
      case _ =>
        Log.info("Fatal error occurred: " + nes.cpu + "-" + t)
        try Log.info(nes.cpu.disassemble(nes.cpuMem,nes.cpu.getCurrentInstructionPC)._1) catch { case _:Throwable => }
        t.printStackTrace(Log.getOut)
        t.printStackTrace
        if (headless) {
          println(s"Fatal error occurred on cycle ${nes.clk.currentCycles}: ${nes.cpu}\n${nes.cpu.disassemble(nes.cpuMem,nes.cpu.getCurrentInstructionPC)._1}")
          t.printStackTrace
          sys.exit(1)
        } // exit if headless
        JOptionPane.showMessageDialog(frame,t.toString + " [PC=" + Integer.toHexString(nes.cpu.getCurrentInstructionPC) + "]", "Fatal error",JOptionPane.ERROR_MESSAGE)
        reset(true)

  protected def reset(play:Boolean,hard:Boolean = false) : Unit =
    nes.clk.pause
    setWarpMode(false)
    if hard then
      diskMenuItem.setEnabled(false)
      changeDiskItem.setEnabled(false)
      ejectDiskItem.setEnabled
      nes.hardResetComponent
      display.blankScreen()
      display.repaint()
      frame.setTitle(s"NESSE v${Version.VERSION} ")
      snapshotMode.setEnabled(false)
      cheatsItem.setEnabled(false)
      nameTableViewerItem.setEnabled(false)
      tileViewerItem.setEnabled(false)
      stateSnapShotManager.clear()
      deviceMenuItem.removeAll()
    else
      nes.resetComponent
    if play then nes.clk.play

  def configure(args: Array[String]) : Unit =
    import Preferences._
    val pref = nes.preferences

    // General
    pref.add(TRACE,"start emulator in tracing mode",false) { trace =>
      if trace then
        traceDialog.forceTracing(true)
        traceDialog.setVisible(true)
    }

    // Joystick
    for j <- 1 to 2 do
      val defaultKeyController = if j == 1 then ControllerSettings.CONTROLLER_1_DEFAULT_KEY_SETTINGS else ControllerSettings.CONTROLLER_2_DEFAULT_KEY_SETTINGS
      pref.add(if j == 1 then JOY1_SETTINGS else JOY2_SETTINGS,s"sets controller $j settings: key|usb:<configuration>",s"${ControllerSettings.settingsString(defaultKeyController)}") { settings =>
          ControllerSettings.parseSettings(settings) match
            case Some(s:KeyControllerSettings) =>
              nes.inputManager.controllers(j - 1).remove()
              val joy = new KeyboardJoystick(j)
              joy.setSettings(s)
              nes.inputManager.controllers(j - 1) = joy
            case Some(s:USBControllerSettings) =>
              nes.inputManager.controllers(j - 1).remove()
              val joy = new USBJoystick(j)
              joy.setSettings(s)
              nes.inputManager.controllers(j - 1) = joy
            case _ =>
              throw new IllegalArgumentException("Bad joystick settings")
      }
    // Devices
    pref.add(KEYBOARD_ON,s"attach Family Keyboard",false) { keybAttached =>
      forceKeyboard = keybAttached
    }
    pref.add(KEYBOARD_LAYOUT_FILE,s"set Family Keyboard's layout file","") { layout =>
      val lay = if layout.nonEmpty then
        FamilyKeyboard.loadLayout(layout) match
          case Some(lay) =>
            lay
          case None =>
            JOptionPane.showMessageDialog(frame,s"Keyboard loading error: bad layout file", "Error",JOptionPane.ERROR_MESSAGE)
            return
      else
        FamilyKeyboard.DEFAULT_LAYOUT

      DeviceFactory.keyboardLayout = lay
      nes.inputManager.getExpansionPortDevices().find(_.inputType == InputType.FamilyKeyboard) match
        case Some(keyb:FamilyKeyboard) =>
          keyb.layout = lay
        case None =>
    }
    pref.add(ZAPPER_ON_PORT,s"insert the zapper into given port","none",Set("1","2","none")) { port =>
      port match
        case "1" => forceZapperOnPort = Some(1)
        case "2" => forceZapperOnPort = Some(2)
        case "none" => forceZapperOnPort = None
    }
    // PPU
    import Cartridge.TV
    pref.add(PPU_TV_MODE,s"sets PPU region","auto",TV.values.map(_.toString).toSet + "auto") { region =>
      tvMode = region match
        case "auto" =>
          nes.getCartridge() match {
            case Some(cart) =>
              nes.ppu.setRegion(cart.ines.tv)
            case None =>
          }
          None
        case r =>
          val region = Cartridge.TV.valueOf(r.toUpperCase())
          nes.ppu.setRegion(region)
          Some(region)
    }
    pref.add(PPU_SCANLINE_EFFECT,s"enable/disable scanline effect",false) { crtEnabled =>
      nes.ppu.enableScanlineEffect(crtEnabled)
    }
    pref.add(PPU_SMOOTH_RENDERING,s"enable/disable smooth rendering",false) { smoothRendering =>
      val hints = if smoothRendering then RenderingHints.VALUE_INTERPOLATION_BICUBIC else RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
      display.setRenderingHints(hints)
    }
    pref.add(PPU_OVERSCAN_ENABLED,s"enable/disable default overscan",true) { overscan =>
      nes.ppu.setIgnoreOverscan(!overscan)
    }
    pref.add(PPU_PALETTE_FILE,s"use a palette file","") { file =>
      try
        Palette.setPaletteFor(PPU.PPUType._2C02,Palette.makePaletteFromBinaryFile(file))
      catch
        case i:IllegalArgumentException =>
          println(s"Invalid palette file: $file")
    }
    // FDS
    pref.add(FDS_ROM_BIOS,s"sets FDS's ROM BIOS path","") { path =>
      val file = new File(path)
      if !file.exists() || file.isDirectory then
        throw new IllegalArgumentException("FDS ROM bios path not valid")
      FDS.setBiosROMFile(path)
    }
    pref.add(FDS_DISK_SCROLL_LOCK_ENABLED,s"disk access turn on/off scroll lock key",false) { scrollLockEnabled =>
      FDS.enableScrollLockAsDiskAccess(scrollLockEnabled)
    }
    // Cheats
    pref.add(CHEATS,s"configure a list of plus sign separated Game Genie's cheat codes","") { cheats =>
      val cl = new ListBuffer[Cheat.Cheat]
      for c <- cheats.split("\\+") do
        Cheat.gameGenieDecode(c.trim) match
          case Some(cheat) =>
            cl += cheat
          case None =>
            println(s"Invalid cheat code $c")
      cheatsFromCommandLine = cl.toList
    }
    // Focus
    pref.add(PAUSE_IF_LOST_FOCUS,s"pauses emulation on focus lost",true) { pauseIfFocusLost => }
    // Check Help
    if (nes.preferences.checkForHelp(args)) {
      println(s"NESSE, NES Scala emulator ver. ${ucesoft.nes.Version.VERSION} (${ucesoft.nes.Version.BUILD_DATE})")
      nes.preferences.printUsage("file to attach")
      sys.exit(0)
    }
    // Parsing
    pref.parseAndLoad(args,nes.configuration)

    // Control panel
    controlPanelDialog = ControlPanel.getDialog(frame,nes,this)

  def run() : Unit = {
    Log.setInfo

    Log.info("Building the system ...")
    // Initialization
    nes.initComponent
    // Inspect
    inspectDialog = InspectPanel.getInspectDialog(frame,nes, () => {
      inspectItem.setSelected(false)
      inspectDialog.setVisible(false)
    })

    systemInitialized = true

    swing {
      val xy = nes.configuration.getProperty(Preferences.XY)
      if (xy == null) frame.setLocationByPlatform(true)
      else {
        try {
          val Array(x, y) = xy.split(",") map { _.toInt }
          frame.setLocation(x,y)
        }
        catch {
          case _ : Throwable =>
            frame.setLocationByPlatform(true)
        }
      }
      frame.setVisible(true)
    }
  }

  protected def buildMenu() : Unit =
    val menubar = new JMenuBar
    frame.setJMenuBar(menubar)

    val fileMenu = new JMenu("File")
    vsMenu = new JMenu("VS")
    vsMenu.setVisible(false)
    val traceMenu = new JMenu("Trace")
    val optionMenu = new JMenu("Options")
    val helpMenu = new JMenu("Help")

    menubar.add(fileMenu)
    menubar.add(vsMenu)
    menubar.add(traceMenu)
    menubar.add(optionMenu)
    menubar.add(helpMenu)

    buildFileMenu(fileMenu)
    buildVSMenu(vsMenu)
    buildOptionsMenu(optionMenu)
    buildTraceMenu(traceMenu)
    buildHelpMenu(helpMenu)

  protected def buildFileMenu(fileMenu:JMenu) : Unit =
    val openCartItem = new JMenuItem("Insert cartridge image ...")
    openCartItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L,java.awt.event.InputEvent.ALT_DOWN_MASK))
    openCartItem.addActionListener(_ => loadCart(None) )
    fileMenu.add(openCartItem)

    val openDiskItem = new JMenuItem("Insert FDS disk image ...")
    openDiskItem.addActionListener(_ => loadFDS(None,false) )
    fileMenu.add(openDiskItem)
    changeDiskItem = new JMenuItem("Change FDS disk image ...")
    changeDiskItem.addActionListener(_ => loadFDS(None,true) )
    fileMenu.add(changeDiskItem)
    changeDiskItem.setEnabled(false)

    ejectDiskItem = new JMenuItem("Eject disk image")
    ejectDiskItem.addActionListener(_ => {
      nes.getCartridge().foreach(_.ejectFDS())
      ejectDiskItem.setEnabled(false)
    }
    )
    fileMenu.add(ejectDiskItem)
    ejectDiskItem.setEnabled(false)

    diskMenuItem = new JMenu("Disk ...")
    diskMenuItem.setEnabled(false)
    fileMenu.add(diskMenuItem)

    fileMenu.addSeparator()

    val saveStateItem = new JMenuItem("Save state ...")
    saveStateItem.addActionListener(_ => saveState(false) )
    saveStateItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S,java.awt.event.InputEvent.ALT_DOWN_MASK))
    fileMenu.add(saveStateItem)
    val loadStateItem = new JMenuItem("Load state ...")
    loadStateItem.addActionListener(_ => loadState(false) )
    fileMenu.add(loadStateItem)
    overwriteLastSavedStateItem = new JMenuItem("Overwrite last state")
    overwriteLastSavedStateItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O,java.awt.event.InputEvent.ALT_DOWN_MASK))
    overwriteLastSavedStateItem.addActionListener(_ => saveState(true) )
    overwriteLastSavedStateItem.setEnabled(false)
    fileMenu.add(overwriteLastSavedStateItem)
    reloadLastSavedStateItem = new JMenuItem("Reload last state")
    reloadLastSavedStateItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L,java.awt.event.InputEvent.ALT_DOWN_MASK|java.awt.event.InputEvent.SHIFT_DOWN_MASK))
    reloadLastSavedStateItem.addActionListener(_ => loadState(true) )
    reloadLastSavedStateItem.setEnabled(false)
    fileMenu.add(reloadLastSavedStateItem)

    val pauseItem = new JCheckBoxMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseItem.addActionListener(_ =>
      paused = pauseItem.isSelected
      if pauseItem.isSelected then
        pause(true)
      else play() )

    fileMenu.add(pauseItem)

    fileMenu.addSeparator()

    deviceMenuItem = new JMenu("Devices")
    fileMenu.add(deviceMenuItem)

    fileMenu.addSeparator()

    val resetItem = new JMenuItem("Reset")
    resetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R,java.awt.event.InputEvent.ALT_DOWN_MASK))
    resetItem.addActionListener(_ => reset(true) )
    fileMenu.add(resetItem)
    val hardResetItem = new JMenuItem("Hard Reset")
    hardResetItem.addActionListener(_ => reset(false,true) )
    hardResetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R,java.awt.event.InputEvent.ALT_DOWN_MASK|java.awt.event.InputEvent.SHIFT_DOWN_MASK))
    fileMenu.add(hardResetItem)

    val prefItem = new JMenuItem("Preferences ...")
    prefItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C,java.awt.event.InputEvent.ALT_DOWN_MASK))
    prefItem.addActionListener(_ => controlPanelDialog.setVisible(true) )
    fileMenu.add(prefItem)

    val exitItem = new JMenuItem("Exit")
    exitItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X,java.awt.event.InputEvent.ALT_DOWN_MASK))
    exitItem.addActionListener(_ => shutdown() )
    fileMenu.add(exitItem)

  protected def buildVSMenu(vsMenu:JMenu) : Unit =
    val insertCoinItem = new JMenuItem("Insert Coin")
    vsMenu.add(insertCoinItem)
    insertCoinItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F3,0))
    insertCoinItem.addActionListener(_ => nes.inputManager.pressInsertCoin() )
    val serviceItem = new JMenuItem("Service button")
    vsMenu.add(serviceItem)
    serviceItem.addActionListener(_ => nes.inputManager.pressService() )
    val blueButton = new JMenuItem("1 Player button (blue)")
    blueButton.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F1,0))
    vsMenu.add(blueButton)
    blueButton.addActionListener(_ => nes.inputManager.pressBlueOrGreenButton(true) )
    val greenButton = new JMenuItem("2 Players button (green)")
    greenButton.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F2,0))
    vsMenu.add(greenButton)
    greenButton.addActionListener(_ => nes.inputManager.pressBlueOrGreenButton(false) )
    val dipsItem = new JMenuItem("Dip switches ...")
    dipsItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4,0))
    dipsItem.addActionListener(_ => {
      for cart <- nes.getCartridge(); game <- cart.ines.game ; vsGame <- game.vsGame do
        val dialog = VsDIPSwitchPanel.getDialog(frame,vsGame.name,vsSwitches,newDips => {
          vsSwitches = newDips
          nes.inputManager.setVS(true,newDips,vsGame.swapControllers)
        })
        paused = true
        pause(true)
        dialog.setVisible(true)
        paused = false
        play()
    })
    vsMenu.add(dipsItem)

  protected def buildOptionsMenu(optionMenu:JMenu) : Unit =
    tileViewerItem = new JMenuItem("Tile viewer ...")
    tileViewerItem.addActionListener(_ => showTileViewer() )
    tileViewerItem.setEnabled(false)
    optionMenu.add(tileViewerItem)

    nameTableViewerItem = new JMenuItem("Name table viewer ...")
    nameTableViewerItem.addActionListener(_ => showNameTableViewer() )
    nameTableViewerItem.setEnabled(false)
    optionMenu.add(nameTableViewerItem)

    val snapshotViewerItem = new JMenuItem("Snapshot viewer ...")
    snapshotViewerItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V,java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotMode = new JCheckBoxMenuItem("Snapshot mode")
    snapshotMode.setEnabled(false)
    snapshotMode.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N,java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotMode.addActionListener(_ => {
      if snapshotMode.isSelected then
        stateSnapShotManager.start()
      else stateSnapShotManager.stop()
    })
    optionMenu.add(snapshotMode)
    snapshotViewerItem.addActionListener( _ => showSnapshowViewer() )
    optionMenu.add(snapshotViewerItem)
    warpModeItem = new JCheckBoxMenuItem("Warp mode")
    warpModeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W,java.awt.event.InputEvent.ALT_DOWN_MASK))
    warpModeItem.setSelected(nes.clk.maximumSpeed)
    warpModeItem.addActionListener(_ => setWarpMode(warpModeItem.isSelected) )
    optionMenu.add(warpModeItem)

    cheatsItem = new JMenuItem("Cheats ...")
    cheatsItem.setEnabled(false)
    cheatsItem.addActionListener(_ => openCheatsDialog())
    optionMenu.add(cheatsItem)

    val fullScreenItem = new JMenuItem("Full screen ...")
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER,java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => fullScreen() )
    optionMenu.add(fullScreenItem)

    val zoom = new JMenu("Zoom")
    optionMenu.add(zoom)
    val zoom1 = new JMenuItem("Zoom x 1")
    zoom1.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_1,java.awt.event.InputEvent.ALT_DOWN_MASK))
    zoom1.addActionListener(_ => zoomDisplay(1) )
    zoom.add(zoom1)
    val zoom2 = new JMenuItem("Zoom x 2")
    zoom2.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_2,java.awt.event.InputEvent.ALT_DOWN_MASK))
    zoom2.addActionListener(_ => zoomDisplay(2) )
    zoom.add(zoom2)
    val zoom3 = new JMenuItem("Zoom x 3")
    zoom3.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_3,java.awt.event.InputEvent.ALT_DOWN_MASK))
    zoom3.addActionListener(_ => zoomDisplay(3) )
    zoom.add(zoom3)

    val snapshotItem = new JMenuItem("Take a picture...")
    snapshotItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_I,java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotItem.addActionListener(_ => takeSnapshot() )
    optionMenu.add(snapshotItem)

  protected def buildTraceMenu(traceMenu:JMenu) : Unit =
    traceItem = new JCheckBoxMenuItem("Trace ...")
    traceItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D,java.awt.event.InputEvent.ALT_DOWN_MASK))
    traceItem.addActionListener(_ => traceDialog.setVisible(traceItem.isSelected) )
    traceMenu.add(traceItem)

    inspectItem = new JCheckBoxMenuItem("Inspect ...")
    inspectItem.addActionListener(_ => inspectDialog.setVisible(inspectItem.isSelected) )
    traceMenu.add(inspectItem)

  protected def buildHelpMenu(helpMenu:JMenu) : Unit =
    val prefItem = new JMenuItem("Command options")
    helpMenu.add(prefItem)
    prefItem.addActionListener(_ => {
      val settingsPanel = new SettingsPanel(nes.preferences)
      JOptionPane.showMessageDialog(frame,settingsPanel,"Command options",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/images/controller.png")))
    })
    val aboutItem = new JMenuItem("About")
    aboutItem.addActionListener(_ => AboutPanel.showAboutDialog(frame))
    helpMenu.add(aboutItem)

  protected def checkDeviceMenu(devices:List[InputDevice]): Unit =
    for d <- devices do
      d.inputType match
        case InputType.FamilyKeyboard =>
          val famkeyb = d.asInstanceOf[FamilyKeyboard]
          val fk = new JMenu("Family Basic")
          val record = new JMenuItem("Tape record")
          val play = new JMenuItem("Tape play")
          val stop = new JMenuItem("Tape stop")
          val loadTape = new JMenuItem("Load tape ...")
          val saveTape = new JMenuItem("Save tape ...")
          saveTape.setEnabled(false)
          fk.add(record)
          fk.add(play)
          fk.add(stop)
          fk.add(loadTape)
          fk.add(saveTape)
          deviceMenuItem.add(fk)
          record.addActionListener(_ => {
            pause()
            famkeyb.setTapeState(FamilyKeyboard.TapeState.RECORD)
            this.play()
          })
          play.addActionListener(_ => {
            pause()
            famkeyb.setTapeState(FamilyKeyboard.TapeState.PLAY)
            this.play()
          })
          stop.addActionListener(_ => {
            pause()
            famkeyb.setTapeState(FamilyKeyboard.TapeState.STOP)
            saveTape.setEnabled(true)
            this.play()
          })
          saveTape.addActionListener(_ => {
            loadSaveDialog(load = false,"Choose where to store tape content",".tape","NESSE Tape",None) match
              case Some(fn) =>
                try
                  famkeyb.save(fn)
                catch
                  case t:Throwable =>
                    JOptionPane.showMessageDialog(frame,s"Tape saving error: ${t.getMessage}", "Error",JOptionPane.ERROR_MESSAGE)
              case None =>
          })
          loadTape.addActionListener(_ => {
            loadSaveDialog(load = true,"Choose from where to load tape content",".tape","NESSE Tape",None) match
              case Some(fn) =>
                try
                  famkeyb.load(fn)
                catch
                  case t:Throwable =>
                    JOptionPane.showMessageDialog(frame,s"Tape loading error: ${t.getMessage}", "Error",JOptionPane.ERROR_MESSAGE)
              case None =>
          })
        case _ =>

  private def openCheatsDialog(): Unit =
    val cheatPanel = new CheatPanel(frame,nes.cpuMem,nes.getCartridge().flatMap(_.ines.game).map(_.name),nes.preferences)
    cheatPanel.dialog.setVisible(true)
    if cheatPanel.isApplied then
      reset(true)

  private def takeSnapshot()  : Unit =
    val fc = new JFileChooser
    fc.showSaveDialog(frame) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
        display.saveSnapshot(file)
      case _ =>
    }

  protected def setWarpMode(on:Boolean) : Unit =
    nes.audioDevice.mute(on)
    nes.clk.maximumSpeed = on
    warpModeItem.setSelected(on)

  protected def pause(displayPause:Boolean = false) : Unit =
    if nes.getCartridge().isDefined then
      nes.clk.pause
      if displayPause then
        display.setPaused

  protected def play() : Unit =
    if nes.getCartridge().isDefined then
      nes.clk.play


  override def fullScreen() : Unit =
    FullScreenMode.goFullScreen(frame,display,PPU.DISPLAY_WIDTH,PPU.DISPLAY_HEIGHT,null,nes.inputManager)

  override def zoomDisplay(factor:Int) : Unit =
    if (factor == 1) pause()
    val xDim = PPU.DISPLAY_WIDTH * factor
    val yDim = PPU.DISPLAY_HEIGHT * factor
    val dim = new Dimension(xDim,yDim)
    display.setPreferredSize(dim)
    display.invalidate
    if (factor == 1) frame.setTitle("")
    frame.pack()
    if (factor == 1) {
      frame.setTitle(s"NESSE v${Version.VERSION} ")
      if (systemInitialized) play()
    }

  override def aspect4_3() : Unit = {
    pause()
    val oldDim = display.getSize()
    val dim = new Dimension((oldDim.height.toDouble * 4 / 3).toInt,oldDim.height)
    display.setPreferredSize(dim)
    display.invalidate
    frame.setTitle("")
    frame.pack()
    frame.setTitle(s"NESSE v${Version.VERSION} ")
    if (systemInitialized) play()
  }

  protected def catchError(activity:String)(block: => Unit) : Unit =
    try
      block
    catch
      case t:Throwable =>
        JOptionPane.showMessageDialog(frame,s"$activity error: ${t.getMessage}", "Error",JOptionPane.ERROR_MESSAGE)
        t.printStackTrace()

  protected def loadSaveDialog(load:Boolean,title:String,fileExt:String,fileExtDescr:String,fileView:Option[javax.swing.filechooser.FileView]): Option[String] =
    val fc = new JFileChooser
    fc.setDialogTitle(title)
    fileView.foreach(fv => fc.setFileView(fv))
    if (lastDirectory != null) fc.setCurrentDirectory(new File(lastDirectory))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f:File) = f.isDirectory ||
        f.getName.toUpperCase.endsWith(fileExt.toUpperCase())
      def getDescription = fileExtDescr
    })
    val choice = if load then fc.showOpenDialog(frame) else fc.showSaveDialog(frame)
    choice match {
      case JFileChooser.APPROVE_OPTION =>
        lastDirectory = fc.getSelectedFile.getParent
        Some(fc.getSelectedFile.toString)
      case _ =>
        None
    }

  protected def setDiskMenu(fds:FDS.FDSFile): Unit =
    diskMenuItem.setEnabled(true)
    diskMenuItem.removeAll()
    val flipSide = new JMenuItem("Flip side")
    flipSide.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,java.awt.event.InputEvent.ALT_DOWN_MASK))
    flipSide.addActionListener(_ => nes.getCartridge().foreach(cart =>
      var currentSide = cart.getDiskSide()
      if fds.diskInfo(currentSide).sideNumber == 0 then
        currentSide += 1
      else
        currentSide -= 1
      currentSide = currentSide % fds.disk.length
      cart.setDiskSide(currentSide)

      diskMenuItem.getMenuComponent(1 + currentSide).asInstanceOf[JRadioButtonMenuItem].setSelected(true)
    ))
    diskMenuItem.add(flipSide)
    val group = new ButtonGroup
    for d <- fds.diskInfo.zipWithIndex do
      val info = d._1
      val item = new JRadioButtonMenuItem(s"Insert disk ${info.diskNumber + 1} side ${if info.sideNumber == 0 then "A" else "B"}")
      if d._2 == 0 then
        item.setSelected(true)
      diskMenuItem.add(item)
      group.add(item)
      item.addActionListener(_ => nes.getCartridge().foreach(_.setDiskSide(d._2)))

  protected def loadFDS(file:Option[String],changeOnly:Boolean = false): Unit =
    if !FDS.isBIOSConfigured() then
      JOptionPane.showMessageDialog(frame, "FDS bios rom not configured", "FDS error", JOptionPane.ERROR_MESSAGE)
      return

    val fds = file match
      case None =>
        loadSaveDialog(load = true,"Loading disk image",".fds","NES disk image",Some(NESFileView))
      case f@Some(_) => f

    fds match
      case None =>
      case Some(disk) =>
        val ld = new LoadingDialog(frame,"Loading Disk ...")
        ld.setVisible(true)
        new Thread("LoadFDS") {
          override def run(): Unit =
            try
              val fdsDisk = FDS.loadFDSFile(disk)
              println(fdsDisk)
              pause()
              if !changeOnly then
                reset(false, true)
                setCartridge(Some(FDS.FDS_INES()))

              setDiskMenu(fdsDisk)
              nes.getCartridge().foreach(_.insertFDS(fdsDisk))
              changeDiskItem.setEnabled(true)
              ejectDiskItem.setEnabled(true)
              frame.setTitle(s"NESSE v${Version.VERSION} ")
            catch
              case i: IllegalArgumentException =>
                JOptionPane.showMessageDialog(frame, i.getMessage(), "Disk error", JOptionPane.ERROR_MESSAGE)
              case t: Throwable =>
                JOptionPane.showMessageDialog(frame, s"Error while loading disk: $t", "Disk error", JOptionPane.ERROR_MESSAGE)
                t.printStackTrace()
            finally
              ld.dispose()
              play()
        }.start()

  protected def loadCart(file:Option[String]) : Unit =
    val cart = file match
      case None =>
        loadSaveDialog(load = true,"Loading cartridge image",".nes","NES cartridge image",Some(NESFileView))
      case f@Some(_) => f

    cart match
      case None =>
      case Some(cart) =>
        val ld = new LoadingDialog(frame,"Loading cartridge ...")
        ld.setVisible(true)
        new Thread("LoadINES") {
          override def run(): Unit =
            try
              val ines = Cartridge.loadINES(cart,tvMode)
              println(ines)
              pause()
              reset(false, true)
              setCartridge(Some(ines))
              diskMenuItem.setEnabled(false)
              diskMenuItem.removeAll()
              frame.setTitle(s"NESSE v${Version.VERSION} ")
            catch
              case i: IllegalArgumentException =>
                JOptionPane.showMessageDialog(frame, i.getMessage(), "Cartridge error", JOptionPane.ERROR_MESSAGE)
              case t: Throwable =>
                JOptionPane.showMessageDialog(frame, s"Error while loading cartridge: $t", "Cartridge error", JOptionPane.ERROR_MESSAGE)
                t.printStackTrace()
            finally
              ld.dispose()
              play()
        }.start()

  protected def setCartridge(_ines:Option[Cartridge.iNES]) : Unit =
    val ines = _ines match
      case Some(ines) => ines
      case None => nes.getCartridge().get.ines

    val devices = _ines match
      case Some(ines) =>
        nes.setCartridge(MapperFactory.mapperFrom(ines,nes.cartIRQHandler,nes.ppu))
      case None =>
        nes.inputManager.getExpansionPortDevices()

    var zapperPresent = devices.exists(e => e.inputType.nameType.endsWith("zapper"))
    // check forced Zapper
    forceZapperOnPort match
      case Some(port) if !zapperPresent =>
        val zapper = new Zapper(port)
        nes.inputManager.addExpansionPortDevice(zapper)
        nes.ppu.setOpticalDevice(zapper)
        zapperPresent = true
      case _ =>
    // check forced keyboard
    if forceKeyboard then
      val keyb = new FamilyKeyboard(DeviceFactory.keyboardLayout)
      nes.inputManager.addExpansionPortDevice(keyb)
    if zapperPresent then
      display.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR))
    else
      display.setCursor(Cursor.getDefaultCursor)

    checkDeviceMenu(devices)
    display.notifyDisplaySizeChange()
    snapshotMode.setEnabled(true)
    cheatsItem.setEnabled(true)
    nameTableViewerItem.setEnabled(true)
    tileViewerItem.setEnabled(true)
    traceDialog.cart = nes.getCartridge().get
    inspectDialog.updateRoot
    // check VS
    vsMenu.setVisible(false)
    for game <- ines.game ; vsgame <- game.vsGame do
      vsMenu.setVisible(true)
      vsSwitches = vsgame.switches
    // reset cheats
    nes.cpuMem.removeAllCheats()
    for c <- cheatsFromCommandLine do
      nes.cpuMem.addCheat(c)

  protected def saveState(overwrite:Boolean): Unit =
    paused = true
    pause()
    var out : ObjectOutputStream = null
    try
      val fn = if overwrite then lastSavedStateFile
      else
        loadSaveDialog(load = false,"Choose where to save current state",".nse","NESSE state files",None) match
          case Some(fn) =>
            if (fn.toUpperCase.endsWith(".NSE")) fn else fn + ".NSE"
          case None =>
            return

      out = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(fn)))
      out.writeObject("NESSE")
      out.writeObject(Version.VERSION)
      out.writeLong(System.currentTimeMillis())
      nes.save(out)
      overwriteLastSavedStateItem.setEnabled(true)
      lastSavedStateFile = fn
    catch
      case t:Throwable =>
        JOptionPane.showMessageDialog(frame,s"Can't save state. Unexpected error occurred: $t","State saving error",JOptionPane.ERROR_MESSAGE)
        t.printStackTrace()
    finally
      if out != null then out.close
      paused = false
      play()

  protected def loadState(reload:Boolean): Unit =
    pause()
    var in : ObjectInputStream = null
    try
      val fn = if reload then lastSavedStateFile
      else loadSaveDialog(load = true,"Choose a state file to load",".nse","NESSE state files",None) match
        case Some(fn) => fn
        case None =>
          return

      in = new ObjectInputStream(new GZIPInputStream(new FileInputStream(fn)))
      if in.readObject().toString != "NESSE" then throw new IllegalArgumentException("Bad state file signature")
      val ver = in.readObject.asInstanceOf[String]
      val ts = in.readLong
      val msg = s"<html><b>Version:</b> $ver<br><b>Date:</b> ${new java.util.Date(ts)}</html>"
      if reload || JOptionPane.showConfirmDialog(frame, msg, "State loading confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION then
        reset(false,true)
        nes.load(in)
        setCartridge(None)
        overwriteLastSavedStateItem.setEnabled(true)
        lastSavedStateFile = fn
        if !reload then
          reloadLastSavedStateItem.setEnabled(true)
        play()
    catch
      case t:Throwable =>
        JOptionPane.showMessageDialog(frame,s"Can't load state. Unexpected error occurred: $t","State loading error",JOptionPane.ERROR_MESSAGE)
        t.printStackTrace()
    finally
      if in != null then in.close
      play()

  private def showSnapshowViewer(): Unit =
    paused = true
    val running = stateSnapShotManager.isRunning
    if running then
      stateSnapShotManager.stop()
    pause()
    SnapshotViewer.getSnapshotViewerDialog(frame,stateSnapShotManager,restoreSnapshot).setVisible(true)
    paused = false
    play()
    if running then
      stateSnapShotManager.start()

  private def restoreSnapshot(snap:StateSnapShotManager.Snap): Unit =
    stateSnapShotManager.getSnapStream(snap.name) match
      case Some(stream) =>
        try
          nes.setSaveSnapShotMode(true)
          nes.load(stream)
        finally
          stream.close()
          nes.setSaveSnapShotMode(false)
      case None =>
        JOptionPane.showMessageDialog(frame,s"Can't load snapshot","Snapshot loading error",JOptionPane.ERROR_MESSAGE)

  private def showNameTableViewer(): Unit =
    if nameTableViewer == null then nameTableViewer = new NameTableViewer(frame,nes.ppu,() => closeFrameListener("NameTableViewer"),() => if !paused then play())
    nameTableViewer.dialog.setVisible(true)
    frameListeners += "NameTableViewer" -> nameTableViewer.updateNameTables
    nes.ppu.setFrameListener(frameUpdate)

  private def showTileViewer(): Unit =
    if tileViewer == null then tileViewer = new TileViewer(frame,nes.ppu,() => closeFrameListener("TileViewer"),() => if !paused then play())
    tileViewer.dialog.setVisible(true)
    frameListeners += "TileViewer" -> tileViewer.updateTiles
    nes.ppu.setFrameListener(frameUpdate)

  private def frameUpdate(): Unit =
    var l = 0
    for l <- frameListeners.values do l()