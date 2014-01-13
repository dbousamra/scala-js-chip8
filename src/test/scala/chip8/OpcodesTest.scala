package chip8

import org.scalatest.FunSpec
import scala.collection.immutable.Stack

class OpcodesTest extends FunSpec {

  val cpu = Cpu(10,
    memory    = Memory.fromData(Array(1, 2, 3, 4, 5)),
    stack     = Stack(1, 2, 3, 4, 5),
    registers = Registers(List.fill[Register](16)(new Register(0)))
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

  describe("8XY5") {
    it("should subtract register Y from register X and store in register X") {
      val registers = cpu.registers.registers.updated(0, Register(10)).updated(1, Register(1))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op8XY5(cpuBefore)(0x8015)
      assert(cpuAfter.registers.registers(0) === Register(9))
    }

    it("should not set the carry flag if register X is less than register Y") {
      val registers = cpu.registers.registers.updated(0, Register(0x01)).updated(1, Register(0x02))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op8XY5(cpuBefore)(0x8015)
      assert(cpuAfter.registers.registers(0) === Register(-1))
      assert(cpuAfter.registers.CARRY === Register(0))
    }
  }

  describe("8XY6") {
    it("should set carry flag if least signficant bit of register X is 1") {
      val registers = cpu.registers.registers.updated(0, Register(0x11)).updated(1, Register(0x02))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op8XY6(cpuBefore)(0x8016)
      assert(cpuAfter.registers.CARRY === Register(1))
    }
  }

  describe("8XY7") {
    it("should set carry flag if register Y is greater than register X") {
      val registers = cpu.registers.registers.updated(0, Register(0x11)).updated(1, Register(0x12))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op8XY7(cpuBefore)(0x8017)
      assert(cpuAfter.registers.CARRY === Register(1))
    }
  }

  describe("8XYE") {
    it("should set carry flag if most signficant bit of register X is 1") {
      val registers = cpu.registers.registers.updated(0, Register(255)).updated(1, Register(2))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op8XYE(cpuBefore)(0x801E)
      assert(cpuAfter.registers.CARRY === Register(1))
    }
    it("should multiply register X by 2") {
      val registers = cpu.registers.registers.updated(0, Register(11)).updated(1, Register(2))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op8XYE(cpuBefore)(0x801E)
      assert(cpuAfter.registers.registers(0) === Register(22))
    }
  }

  describe("9XY0") {
    it("should increase program counter by 2 if register X does not equal register Y") {
      val registers = cpu.registers.registers.updated(0, Register(0)).updated(1, Register(1))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.op9XY0(cpuBefore)(0x9010)
      assert(cpuAfter.pc == cpuBefore.pc + 2)
    }
  }

  describe("BNNN") {
    it("should set the program counter to NNN plus the value in register 0") {
      val registers = cpu.registers.registers.updated(0, Register(1))
      val cpuBefore = cpu.copy(registers = Registers(registers))
      val cpuAfter = Opcodes.opBNNN(cpuBefore)(0x9300)
      assert(cpuAfter.pc == 0x301)
    }
  }

//  describe("CXNN") {
//    it("should set the register X to a random number AND'ed with NN") {
//      val opcode = 0xC011
//      val registers = cpu.registers.registers.updated(0, Register(1))
//      val cpuBefore = cpu.copy(registers = Registers(registers))
//      val cpuAfter = Opcodes.opCXNN(cpuBefore, 100)
//      assert(cpuAfter.registers.X === Register(100 & (opcode & 0x00FF)))
//    }
//  }

  describe("FX07") {
    it("should set the the register X to the delay timer value") {
      val registers = cpu.registers.registers.updated(0, Register(1))
      val cpuBefore = cpu.copy(registers = Registers(registers), delayTimer = 10)
      val cpuAfter = Opcodes.opFX07(cpuBefore)(0xF007)
      assert(cpuAfter.registers.registers(0) === Register(cpuBefore.delayTimer))
    }
  }

  describe("FX15") {
    it("should set the the delay timer to the value in register X") {
      val registers = cpu.registers.registers.updated(0, Register(22))
      val cpuBefore = cpu.copy(registers = Registers(registers), delayTimer = 10)
      val cpuAfter = Opcodes.opFX15(cpuBefore)(0xF015)
      assert(cpuAfter.delayTimer === cpuBefore.registers.registers(0).value)
    }
  }

  describe("FX18") {
    it("should set the the sound timer to the value in register X") {
      val registers = cpu.registers.registers.updated(0, Register(22))
      val cpuBefore = cpu.copy(registers = Registers(registers), soundTimer = 10)
      val cpuAfter = Opcodes.opFX18(cpuBefore)(0xF018)
      assert(cpuAfter.soundTimer === cpuBefore.registers.registers(0).value)
    }
  }

  describe("FX1E") {
    it("should add the values in register I and register X and store them in register I") {
      val registers = cpu.registers.registers.updated(0, Register(22))
      val cpuBefore = cpu.copy(registers = Registers(registers), registerI = Register(11))
      val cpuAfter = Opcodes.opFX1E(cpuBefore)(0xF01E)
      assert(cpuAfter.registerI === Register(33))
    }
  }

  describe("FX55") {
    it("should store registers 0 through registers X in memory starting at location I.") {
      val registers = cpu.registers.registers.updated(0, Register(3)).updated(1, Register(5)).updated(2, Register (7))
      val cpuBefore = cpu.copy(registers = Registers(registers), registerI = Register(10))
      val cpuAfter = Opcodes.opFX55(cpuBefore)(0xF055)
      assert(cpuAfter.memory.data.slice(10, 13) === Array(3, 5, 7))
    }
  }

  describe("FX65") {
    it("Read registers from register 0 through X from memory into registers 0 through X starting at location I") {
      val registers = cpu.registers.registers.updated(0, Register(3)).updated(1, Register(5)).updated(2, Register (7))
      val cpuBefore = cpu.copy(registers = Registers(registers), registerI = Register(0))
      val cpuAfter = Opcodes.opFX65(cpuBefore)(0xF065)
      assert(cpuAfter.registers === List(Register(1), Register(2), Register(3)))
    }
  }
}
