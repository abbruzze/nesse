package ucesoft.nes.cpu

object Bits:
  def hex2(data: Int) : String = "%02X".format(data & 0xffff)
  def hex4(data: Int) : String = "%04X".format(data & 0xffff)

  extension (s : String)
    def b : Int = Integer.parseInt(s, 2)
    def x : Int = Integer.parseInt(s, 16)

  def word(address:Int,mem:Memory) : Int =
    val lo = mem.read(address)
    val hi = mem.read(address + 1)
    hi << 8 | lo
  
