package ucesoft.nes.sound

import ucesoft.nes.NESComponent

import java.io.{ObjectInputStream, ObjectOutputStream}

/**
 * A sequencer generates a series of values or events based on the repetition of a
 * series of steps, starting with the first. When clocked the next step of the
 * sequence is generated.
 */
class Sequencer(var sequence : Array[Int]):
  private var sequenceCounter = 0

  def setSequence(sequence: Array[Int]): Unit =
    this.sequence = sequence
    sequenceCounter = sequenceCounter % sequence.length

  def step(): Unit =
    sequenceCounter = (sequenceCounter + 1) % sequence.length

  def output: Int = sequence(sequenceCounter)

  def reset(): Unit =
    sequenceCounter = 0

  def saveState(out: ObjectOutputStream) : Unit =
    out.writeObject(sequence)
    out.writeInt(sequenceCounter)

  def loadState(in:ObjectInputStream) : Unit =
    NESComponent.loadMemory(sequence,in)
    sequenceCounter = in.readInt()
  


