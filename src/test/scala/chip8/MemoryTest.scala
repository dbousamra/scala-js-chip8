package chip8

import org.scalatest.FlatSpec

class MemoryTest extends FlatSpec {

  "Memory" should "be the fontSet from 0x000 onwards" in {
    val memory = Memory.fromData(Array(1, 2, 3))
    assert(memory.data.slice(0x000, Memory.fontSet.length) === Memory.fontSet)
  }

  "Memory" should "be empty from the end of the fontSet to 0x200" in {
    val memory = Memory.fromData(Array(1, 2, 3))
    assert(memory.data.slice(Memory.fontSet.length, 0x200).forall(_ == 0) === true)
  }
}
