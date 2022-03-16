package ucesoft.nes.cpu

import ucesoft.nes.ChipID

trait Memory {
  val name: String

  private[this] var forwardRead,forwardWrite = false
  protected[this] var forwardReadTo,forwardWriteTo : Memory = null

  final def isForwardRead = forwardRead
  final def isForwardWrite = forwardWrite
  final def setForwardReadTo(forwardReadTo:Option[Memory]) : Unit = {
    this.forwardReadTo = forwardReadTo match {
      case Some(fr) =>
        forwardRead = true
        fr
      case None =>
        forwardRead = false
        null
    }
  }
  final def setForwardWriteTo(forwardWriteTo:Option[Memory]) : Unit = {
    this.forwardWriteTo = forwardWriteTo match {
      case Some(fw) =>
        forwardWrite = true
        fw
      case None =>
        forwardWrite = false
        null
    }
  }

  def init : Unit = {}
  def read(address: Int, chipID: ChipID = ChipID.CPU): Int
  def peek(address: Int, chipID: ChipID = ChipID.CPU): Int = read(address, chipID)
  def write(address: Int, value: Int, chipID: ChipID = ChipID.CPU) : Unit
  def load(address: Int, value: Int, chipID: ChipID = ChipID.CPU) : Unit = {}
  def byteOnBUS : Int = 0

  override def toString = s"$name"
}

object Memory {
  val empty = dummyWith(0,0)

  def dummyWith(startAddress:Int,values:Int*) = new Memory {
    private val mem = values.toArray
    val name = "DUMMY"

    def read(address: Int, chipID: ChipID = ChipID.CPU) = mem(address - startAddress)
    def write(address: Int, value: Int, chipID: ChipID = ChipID.CPU) = mem(address - startAddress) = value
  }
}
