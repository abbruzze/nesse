package ucesoft.nes.mappers.chips

import ucesoft.nes.NESComponent

import java.io.{ObjectInputStream, ObjectOutputStream}

class _24C02:
  private enum State:
    case IDLE,CONTROL_CODE,ADDRESS,DATA,READ

  import State.*

  private[this] val eeprom = Array.ofDim[Int](256)
  private[this] val writeBuffer = Array.ofDim[Int](16)
  private[this] var writeBufferCount = 0
  private[this] var scl,sda,sda_out = true
  private[this] var address,data,code = 0
  private[this] var bitCount = 0
  private[this] var state = IDLE
  private[this] var ack = false
  
  def saveEEPROM(out:ObjectOutputStream):Unit = out.writeObject(eeprom)
  def loadEEPROM(in:ObjectInputStream):Unit = NESComponent.loadMemory(eeprom,in)
  
  def saveState(out:ObjectOutputStream):Unit =
    out.writeObject(eeprom)
    out.writeObject(writeBuffer)
    out.writeInt(writeBufferCount)
    out.writeBoolean(scl)
    out.writeBoolean(sda)
    out.writeBoolean(sda_out)
    out.writeInt(address)
    out.writeInt(data)
    out.writeInt(code)
    out.writeInt(bitCount)
    out.writeInt(state.ordinal)
    out.writeBoolean(ack)
    
  def loadState(in:ObjectInputStream):Unit =
    NESComponent.loadMemory(eeprom,in)
    NESComponent.loadMemory(writeBuffer,in)
    writeBufferCount = in.readInt()
    scl = in.readBoolean()
    sda = in.readBoolean()
    sda_out = in.readBoolean()
    address = in.readInt()
    data = in.readInt()
    code = in.readInt()
    bitCount = in.readInt()
    state = State.fromOrdinal(in.readInt())
    ack = in.readBoolean()

  def reset(): Unit =
    java.util.Arrays.fill(eeprom,0)
    java.util.Arrays.fill(writeBuffer,0)
    writeBufferCount = 0
    address = 0
    data = 0
    bitCount = 0
    scl = true
    sda = true
    sda_out = true
    state = IDLE
    ack = false

  def readSDA: Int = if sda_out then 1 else 0

  private def write():Unit =
    if writeBufferCount == 1 then
      eeprom(address) = writeBuffer(0)
      //println("EEPROM WRITE1 [" + address + "] = " + eeprom(address))
      address = (address + 1) & 0xFF
    else
      val writeSize = math.max(writeBufferCount,16)
      var c = 0
      while c < writeSize do
        eeprom(address) = writeBuffer(c)
        //println("EEPROM WRITEn [" + address + "] = " + eeprom(address))
        address = (address & 0xFF00) | (((address & 0xFF) + 1) & 0xFF)
        c += 1

  def clock(_scl:Boolean,_sda:Boolean): Unit =
    if _scl && scl then // clock High
      if sda && !_sda then // START condition
        state = CONTROL_CODE
        bitCount = 0
        code = 0
      else if !sda && _sda then // STOP condition
        if writeBufferCount > 0 then
          write()
          writeBufferCount = 0
        state = IDLE
    else if !scl && _scl then // clock rising edge
      state match
        case CONTROL_CODE =>
          bitCount += 1
          if bitCount == 8 then
            //println("EEPROM ADDRESS " + code)
            state = if _sda then READ else ADDRESS // 9th bit R/_W
            bitCount = 0
            ack = true
          else code = (code << 1) | (if _sda then 1 else 0)
        case ADDRESS =>
          if ack then
            ack = false
            sda_out = false
          else
            bitCount += 1
            address = (address << 1) | (if _sda then 1 else 0)
            if bitCount == 8 then
              bitCount = 0
              address &= 0xFF
              //println("EEPROM WORD " + address)
              ack = true
              state = DATA
        case DATA =>
          if ack then
            ack = false
            sda_out = false
          else
            bitCount += 1
            data = (data << 1) | (if _sda then 1 else 0)
            if bitCount == 8 then
              bitCount = 0
              data &= 0xFF
              ack = true
              //eeprom(address) = data
              //println("EEPROM WRITE [" + address + "] = " + data)
              writeBuffer(writeBufferCount % 16) = data
              writeBufferCount += 1
              //address = (address + 1) & 0xFF
        case READ =>
          if ack then
            ack = false
            sda_out = false
          else
            if bitCount == 0 then
              data = eeprom(address)
              //println("EEPROM READ [" + address + "] = " + data)
            bitCount += 1
            sda_out = (data >> 7) > 0
            data = (data << 1) & 0xFF
            if bitCount == 8 then
              bitCount = 0
              address = (address + 1) & 0xFF
        case _ =>

    scl = _scl
    sda = _sda
