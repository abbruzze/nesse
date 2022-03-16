package ucesoft.nes

import ucesoft.nes.controller.*
import ucesoft.nes.cpu.CPU6502
import ucesoft.nes.mappers.MapperFactory
import ucesoft.nes.misc.Preferences
import ucesoft.nes.sound.{APU, AudioDevice}

import java.io.*
import java.util.Properties

class NES(errorHandler: Throwable => Unit) extends NESComponent:
  override val componentID: String = "NES Motherboard"
  override val componentType: NESComponentType = NESComponentType.MOTHERBOARD
  protected val CONFIGURATION_FILENAME = "NESSE.config"

  // PREFERENCES ===============================================================
  val preferences = new Preferences
  val configuration = new Properties()
  private[this] var _configurationFile : File = _

  val clk = Clock.setSystemClock(Some(errorHandler),autoTick = true)(loop)
  val audioDevice = new AudioDevice(44100)
  val inputManager = new InputManager
  val ppu = new PPU(triggerNMI)
  val apu = new APU(apuIRQHandler,dmaReadSampleHandler,audioDevice)
  val cpuMem = new CPUMemory(startDMA,ppu,apu,inputManager)
  val cpu = new CPU6502(cpuMem,ChipID.CPU)

  private[this] var cpuDividerCounter = 0f
  private[this] var cpuDivider = 0f
  private[this] var dmaAddress,dmaLow = 0
  private[this] var dmaRead = true
  private[this] var dmaON = false
  private[this] var dmaLatch = 0
  private[this] var dmcDMACount = 0
  private[this] var dmcAddress = 0
  private[this] var apuIRQ, cartIRQ = false
  
  private[this] var saveSnapshotMode = false

  private[this] var cart : Cartridge = _

  // CONSTRUCTOR ===============================================================
  configure()
  // ===========================================================================
  
  def setSaveSnapShotMode(on:Boolean) : Unit = saveSnapshotMode = on

  def configurationFile() : File = _configurationFile

  def saveConfiguration() : Unit = {
    val conf = new FileWriter(configurationFile())
    configuration.store(conf,"NESSE configuration file")
    conf.close()
  }

  def savePreferences() : Unit = {
    preferences.save(configuration)
    saveConfiguration()
  }

  private def configure() : Unit =
    // LOAD configuration
    // NESSE HOME ==========================================================================
    var nesseHome = System.getProperty("nesse.home")
    if (nesseHome == null) {
      nesseHome = scala.util.Properties.userHome
      println(s"Warning: nesse.home env variable not set. Default nesse home is $nesseHome")
    }
    else nesseHome = new File(new File(nesseHome),"conf").toString
    // PROPERTIES =======================================================================

    val configHome = System.getProperty("nesse.config",nesseHome)
    _configurationFile = new File(new File(configHome),CONFIGURATION_FILENAME)
    if (_configurationFile.exists) {
      try {
        configuration.load(new FileReader(_configurationFile))
      }
      catch {
        case _:IOException =>
          setDefaultProperties()
      }
    }
    else setDefaultProperties()

  protected def setDefaultProperties() : Unit = {}

  def setCartridge(cart:Cartridge) : List[InputDevice] =
    if this.cart != null then
      remove(this.cart)
      this.cart.eject()

    this.cart = cart
    cart.init
    apu.setCartridge(cart)
    ppu.setCartridge(cart)
    cpuMem.setCartridge(cart)
    clk.setClockHz(cart.ines.tv.ppuClockFrequency)
    audioDevice.setCPUFrequency(cart.ines.tv.ppuClockFrequency / cart.ines.tv.cpuDivider)
    cpuDivider = cart.ines.tv.cpuDivider
    cpuDividerCounter = cpuDivider
    add(cart)
    // check VS
    inputManager.setVS(false)
    for game <- cart.ines.game ; vsGame <- game.vsGame do
      inputManager.setVS(true,vsGame.switches,vsGame.swapControllers)
    // check devices
    inputManager.removeAllExpansionPortDevices()
    val devices : List[InputDevice] = cart.ines.game match
      case None =>
        DeviceFactory.deviceFrom(cart.ines.crc,cpuMem,inputManager.controllers(0),inputManager.controllers(1)) match
          case None =>
            Nil
          case Some(device) =>
            if device.isOpticalDevice then
              ppu.setOpticalDevice(device)
            inputManager.addExpansionPortDevice(device)
            device :: Nil
      case Some(game) =>
        for dev <- game.devices yield
          val device = DeviceFactory.deviceFrom(dev,game,cpuMem,inputManager.controllers(0),inputManager.controllers(1))
          if device.isOpticalDevice then
            ppu.setOpticalDevice(device)
          inputManager.addExpansionPortDevice(device)
          device
    devices

  def removeCartridge() : Unit =
    if cart != null then
      remove(this.cart)
      cart.eject()
      cart = null

  def getCartridge() : Option[Cartridge] = Option(cart)

  private def dmaReadSampleHandler(address:Int) : Int =
    dmcDMACount = 4 + (if dmaON then 2 else 0)
    dmcAddress = address
    cpuMem.read(address)

  private def startDMA(page:Int) : Unit =
    dmaON = true
    dmaAddress = page << 8
    dmaLow = 0
    dmaRead = true

  private def triggerNMI() : Unit =
    cpu.nmiRequest(true)
    cpu.nmiRequest(false)

  private def apuIRQHandler(low:Boolean) : Unit =
    apuIRQ = low
    checkIRQ()

  def cartIRQHandler(low:Boolean) : Unit =
    cartIRQ = low
    checkIRQ()

  inline private def checkIRQ() : Unit =
    cpu.irqRequest(apuIRQ | cartIRQ)

  private def loop(cycles:Long) : Unit = {
    cpuDividerCounter -= 1
    if cpuDividerCounter <= 0 then
      cpuDividerCounter += cpuDivider
      if dmcDMACount > 0 then
        dmcDMACount -= 1
        if dmcDMACount == 0 then
          apu.setSample(cpuMem.read(dmcAddress))
      else
        if dmaON then
          dmaCycle
        else
          cpu.clock()

      //inputManager.controllers(0).clock()
      //inputManager.controllers(1).clock()
      inputManager.clock()
      apu.clock()
      cart.cpuClock()
    end if
    ppu.clock()
  }

  private def dmaCycle : Unit =
    if dmaRead then
      dmaLatch = cpuMem.read(dmaAddress | dmaLow,ChipID.CPU)
    else
      ppu.write(0x2004,dmaLatch) // OAMDATA
      dmaLow = (dmaLow + 1) & 0xFF
      if dmaLow == 0 then
        dmaON = false
        //cpu.setDMA(false)
    dmaRead ^= true

  override def reset: Unit =
    dmaRead = true
    dmaON = false
    dmcDMACount = 0
    apuIRQ = false
    cartIRQ = false
    if cart != null then
      cpuDivider = cart.ines.tv.cpuDivider
      cpuDividerCounter = cpuDivider

  override def hardReset: Unit =
    inputManager.removeAllExpansionPortDevices()
    removeCartridge()
    reset

  override def init: Unit =
    add(inputManager)
    add(audioDevice)
    add(clk)
    add(apu)
    add(ppu)
    add(cpu)
    add(cpuMem)

    audioDevice.start()

  override def saveState(out: ObjectOutputStream): Unit =
    if !saveSnapshotMode then
      out.writeObject(cart.ines)

  override def loadState(in: ObjectInputStream): Unit =
    if !saveSnapshotMode then
      val ines = in.readObject().asInstanceOf[Cartridge.iNES]
      setCartridge(MapperFactory.mapperFrom(ines,cartIRQHandler,ppu))
