package chip8

import org.scalatest.FunSpec
import scala.collection.immutable.Stack

class OpcodesTest extends FunSpec {

  val cpu = Cpu(10,
    Memory.fromData(Array(1, 2, 3, 4, 5)),
    Stack(1, 2, 3, 4, 5),
    Registers(List.fill[Register](16)(new Register(0)))
  )

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
    val cpuBefore = cpu.copy(registers = Registers(List(Register(22), Register(33))))

    it("should skip the next instruction if register at X is equal to register at Y") {
      val cpuAfter = Opcodes.op5XY0(cpuBefore)(0x5000)
      assert(cpuAfter.pc === cpuBefore.pc + 2)
    }

    it("should not skip the next instruction if register at X is not equal to register at Y") {
      val cpuAfter = Opcodes.op5XY0(cpuBefore)(0x5010)
      assert(cpuAfter.pc === cpuBefore.pc)
    }
  }

  describe("6XNN") {
    it("should put the value at NN into the register at X") {
      val cpuAfter = Opcodes.op6XNN(cpu)(0x6022)
      assert(cpuAfter.registers.registers(0) === Register(0x22))
    }
  }

  describe("7XNN") {
    it("should add the value NN to the value in register X") {
      val cpuBefore = cpu.copy(registers = Registers(List(Register(0x10))))
      val cpuAfter = Opcodes.op7XNN(cpuBefore)(0x7022)
      assert(cpuAfter.registers.registers(0) === Register(0x32))
    }
  }

  describe("8XY0") {
    it("should store the value of register Y in register X.") {
      val cpuBefore = cpu.copy(registers = Registers(List(Register(0x00), Register(0x01))))
      val cpuAfter = Opcodes.op8XY0(cpuBefore)(0x8010)
      assert(cpuAfter.registers.registers(0) === Register(0x01))
    }
  }

  describe("8XY1") {
    it("should store the result of a bitwise OR on the values in registers X and Y, and stores the result in register X") {
      val cpuBefore = cpu.copy(registers = Registers(List(Register(0x05), Register(0x06))))
      val cpuAfter = Opcodes.op8XY1(cpuBefore)(0x8011)
      assert(cpuAfter.registers.registers(0) === Register(0x07))
    }
  }

  describe("8XY2") {
    it("should store the result of a bitwise AND on the values in registers X and Y, and stores the result in register X") {
      val cpuBefore = cpu.copy(registers = Registers(List(Register(0x00), Register(0x01))))
      val cpuAfter = Opcodes.op8XY2(cpuBefore)(0x8012)
      assert(cpuAfter.registers.registers(0) === Register(0x00))
    }
  }

  describe("8XY3") {
    it("should store the result of an XOR on the values in registers X and Y, and stores the result in register X") {
      val cpuBefore = cpu.copy(registers = Registers(List(Register(0x00), Register(0x01))))
      val cpuAfter = Opcodes.op8XY3(cpuBefore)(0x8013)
      assert(cpuAfter.registers.registers(0) === Register(0x01))
    }
  }

  describe("8XY4") {
    it("should store the value of register Y in register X added together, but only lower 8 bits") {
      val registers = cpu.registers.registers.updated(0, Register(33)).updated(1, Register(1))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op8XY4(cpuBefore)(0x8014)
      assert(cpuAfter.registers.registers(0) === Register(34))
    }

    it("should store the value of register Y in register X added together, but only lower 8 bits for greater than 255") {
      val registers = cpu.registers.registers.updated(0, Register(255)).updated(1, Register(6))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op8XY4(cpuBefore)(0x8014)
      assert(cpuAfter.registers.registers(0) === Register(5))
      assert(cpuAfter.registers.CARRY === Register(1))
    }
  }
//
//  describe("8XY5") {
//    it("should store the value of register Y List register X.") {
//      val cpuBefore = cpu.copy(registers = List(Register(0x00), Register(0x01)))
//      val cpuAfter = Opcodes.op8XY0(cpuBefore)(0x7010)
//      assert(cpuAfter.registers(0) === Register(0x07))
//    }
//  }
//
//  describe("8XY6") {
//    it("should store the value of register Y in register X.") {
//      val cpuBefore = cpu.copy(registers = List(Register(0x00), Register(0x01)))
//      val cpuAfter = Opcodes.op8XY0(cpuBefore)(0x7010)
//      assert(cpuAfter.registers(0) === Register(0x01))
//    }
//  }
//
//  describe("8XY7") {
//    it("should store the value of register Y in register X.") {
//      val cpuBefore = cpu.copy(registers = List(Register(0x00), Register(0x01)))
//      val cpuAfter = Opcodes.op8XY0(cpuBefore)(0x7010)
//      assert(cpuAfter.registers(0) === Register(0x01))
//    }
//  }
//
//  describe("8XYE") {
//    it("should store the value of register Y in register X.") {
//      val cpuBefore = cpu.copy(registers = List(Register(0x00), Register(0x01)))
//      val cpuAfter = Opcodes.op8XY0(cpuBefore)(0x7010)
//      assert(cpuAfter.registers(0) === Register(0x01))
//    }
//  }


}
