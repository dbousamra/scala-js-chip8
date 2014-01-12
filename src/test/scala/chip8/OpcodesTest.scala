package chip8

import org.scalatest.FunSpec
import scala.collection.immutable.Stack

class OpcodesTest extends FunSpec {

  val cpu = Cpu(10,
    Memory(Array(1, 2, 3, 4, 5)),
    Stack(1, 2, 3, 4, 5))

  describe("00EE") {
    val cpuAfter = Opcodes.op00EE(cpu)(0x00EE)

    it("should push program counter onto stack") {
      assert(cpuAfter.pc === cpu.stack.head)
    }

    it("should pop head off stack off") {
      assert(cpuAfter.stack.head === cpu.stack(1))
    }
  }

  describe("1NNN") {
    val cpuAfter = Opcodes.op1NNN(cpu)(0x1100)

    it("should push program counter onto stack") {
      assert(cpuAfter.pc === 0x100)
    }
  }

  describe("2NNN") {
    val cpuAfter = Opcodes.op2NNN(cpu)(0x2001)

    it("should push program counter onto stack") {
      assert(cpuAfter.stack.head === cpu.pc)
    }
  }

  describe("3XNN") {
    val cpuAfter = Opcodes.op3XNN(cpu)(0x3100)

    it("should skip the next instruction if register at X is equal to NN") {
      assert(cpuAfter.pc === cpu.pc + 2)
    }

    it("should skip not skip the next instruction if register at X is not equal to NN") {
      val cpuAfterWithoutSkipping = Opcodes.op3XNN(cpu)(0x3101)
      assert(cpuAfterWithoutSkipping.pc === cpu.pc)
    }
  }

  describe("4XNN") {
    it("should skip not skip the next instruction if register at X is equal to NN") {
      val cpuAfter = Opcodes.op4XNN(cpu)(0x3100)
      assert(cpuAfter.pc === cpu.pc)
    }

    it("should skip the next instruction if register at X is not equal to NN") {
      val cpuAfter = Opcodes.op4XNN(cpu)(0x3101)
      assert(cpuAfter.pc === cpu.pc + 2)
    }
  }

  describe("5XY0") {
    val cpuBefore = cpu.copy(registers = List(Register(22), Register(33)))

    it("should skip the next instruction if register at X is equal to register at Y") {
      val cpuAfter = Opcodes.op5XY0(cpuBefore)(0x5000)
      assert(cpuAfter.pc === cpuBefore.pc + 2)
    }

    it("should not skip the next instruction if register at X is not equal to register at Y") {
      val cpuAfter = Opcodes.op5XY0(cpuBefore)(0x5010)
      assert(cpuAfter.pc === cpuBefore.pc)
    }
  }

  describe("6XKK") {
    it("should put the value at KK into the register at X") {
      val cpuAfter = Opcodes.op6XKK(cpu)(0x6022)
      assert(cpuAfter.registers(0) === Register(0x22))
    }
  }
}
