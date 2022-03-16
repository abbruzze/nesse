package ucesoft.nes

import ucesoft.nes.controller.InputDevice
import ucesoft.nes.cpu.{CPU6502, Memory}
import ucesoft.nes.util.Cheat

import java.io.{ObjectInputStream, ObjectOutputStream}
import scala.collection.mutable.ListBuffer

class CPUMemory(startDMA:Int => Unit,
                ppuMem:Memory,
                apuMem:Memory,
                inputManager:InputDevice) extends NESComponent with Memory {
  override val name: String = "CPUMemory"
  override val componentID: String = "CPUMemory"
  override val componentType: NESComponentType = NESComponentType.MEMORY

  private[this] val ram = Array.ofDim[Int](2048)
  private[this] var cart : Cartridge = _
  private[this] var cartListenToAllAddresses = false
  // Cheats
  private[this] val cheatAddress = Array.ofDim[Cheat.Cheat](0x8000)
  private[this] val cheatList = new ListBuffer[Cheat.Cheat]

  def setCartridge(cart:Cartridge) : Unit =
    this.cart = cart
    cartListenToAllAddresses = cart.listenToAllAddresses

  def addCheat(cheat:Cheat.Cheat): Unit =
    cheatAddress(cheat.address - 0x8000) = cheat
    cheatList += cheat

  def removeCheat(cheat:Cheat.Cheat): Unit =
    cheatAddress(cheat.address - 0x8000) = null
    cheatList -= cheat

  def removeAllCheats(): Unit =
    for a <- cheatAddress.indices do cheatAddress(a) = null
    cheatList.clear()
    
  def getCheats(): List[Cheat.Cheat] = cheatList.toList

  override def reset: Unit = {}

  override def hardReset: Unit =
    java.util.Arrays.fill(ram,0)

  override def read(address: Int, chipID: ChipID): Int =
    var readFromCart = false
    val valueRead =
    // 2K RAM
    if address < 0x2000 then ram(address & 0x7FF)
    else
    // PPU 8 internal registers
    if address < 0x4000 then ppuMem.read(address & 7)
    else
    if address == 0x4015 then
      apuMem.read(address)
    else
    if address == 0x4016 then inputManager.readPort(1)
    else
    if address == 0x4017 then inputManager.readPort(2)
    // cart memory
    else
      readFromCart = true
      var read = cart.read(address, chipID)
      if address > 0x7FFF && cheatAddress(address - 0x8000) != null then
        val cheat = cheatAddress(address - 0x8000)
        cheat.compare match
          case None =>
            read = cheat.data
          case Some(cmp) =>
            if read == cmp then read = cheat.data
      read

    if !readFromCart && cartListenToAllAddresses then
      cart.read(address, chipID)

    valueRead

  override def write(address: Int, value: Int, chipID: ChipID): Unit =
    var wroteToCart = false
    // 2K RAM
    if address < 0x2000 then ram(address & 0x7FF) = value & 0xFF
    else
    // PPU 8 internal registers
    if address < 0x4000 then ppuMem.write(address & 7,value,chipID)
    else
    // DMA register
    if address == 0x4014 then startDMA(value)
    else
    if address == 0x4015 then
      apuMem.write(address,value)
    else
    if address == 0x4016 then
      inputManager.writePort(1,value)
    else
    if address == 0x4017 then
      inputManager.writePort(1,value)
      apuMem.write(address,value)
    else
    // APU registers
    if address < 0x4020 then
      apuMem.write(address,value)
    // cart memory
    else
      wroteToCart = true
      cart.write(address, value, chipID)

    if !wroteToCart && cartListenToAllAddresses then
      cart.write(address, value, chipID)

  override def saveState(out: ObjectOutputStream): Unit =
    out.writeObject(ram)

  override def loadState(in: ObjectInputStream): Unit =
    loadMemory(ram,in)
}
