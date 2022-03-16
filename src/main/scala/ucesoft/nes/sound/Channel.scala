package ucesoft.nes.sound

import ucesoft.nes.Cartridge

import java.io.{ObjectInputStream, ObjectOutputStream}

trait Channel extends Clocked:
  def setEnabled(enabled: Boolean): Unit
  def setRegister(index: Int, value: Int): Unit
  def output(): Int
  def isActive(): Boolean

  def clockLengthCounterAndSweep(): Unit
  def clockEnvelopAndLinearCounter(): Unit

  def getInterruptFlag() : Boolean = false

  def reset() : Unit
  
  def saveState(out:ObjectOutputStream) : Unit
  def loadState(in:ObjectInputStream) : Unit
  
  def setTV(tv:Cartridge.TV): Unit = {}
