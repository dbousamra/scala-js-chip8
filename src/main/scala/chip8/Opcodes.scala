package chip8

import scala.util.Random
import scala.NotImplementedError

object Opcodes {
  def fetch(opcode: Opcode): CpuReader = (opcode.instruction match {
    case 0x0000 => opcode & 0xF match {
      case 0x0000 => clearScreen
      case 0x000E => op00EE
    }
    case 0x1000 => op1NNN
    case 0x2000 => op2NNN
    case 0x3000 => op3XNN
    case 0x4000 => op4XNN
    case 0x5000 => op5XY0
    case 0x6000 => op6XNN
    case 0x7000 => op7XNN
    case 0x8000 => opcode & 0xF match {
      case 0x0 => op8XY0
      case 0x1 => op8XY1
      case 0x2 => op8XY2
      case 0x3 => op8XY3
      case 0x4 => op8XY4
      case 0x5 => op8XY5
      case 0x6 => op8XY6
      case 0x7 => op8XY7
      case 0xE => op8XYE
    }
    case 0x9000 => op9XY0
    case 0xA000 => opANNN
    case 0xB000 => opBNNN
    case 0xC000 => opCXNN
    case 0xD000 => opDXYN
    case 0xE000 => opcode & 0xF match {
      case 0xE => opEX9E
      case 0x1 => opEXA1
    }
    case 0xF000 => opcode & 0xFF match {
      case 0x07 => opFX07
      case 0x0A => opFX0A
      case 0x15 => opFX15
      case 0x18 => opFX18
      case 0x1E => opFX1E
      case 0x29 => opFX29
      case 0x33 => opFX33
      case 0x55 => opFX55
      case 0x65 => opFX65
    }
    case _ => throw new NotImplementedError
  })(opcode)

  val clearScreen: CpuOp = _ => identity

  val op00EE: CpuOp = opcode => cpu => cpu.copy(pc = cpu.stack.head, stack = cpu.stack.pop)

  val op1NNN: CpuOp = opcode => _.copy(pc = opcode.NNN)

  val op2NNN: CpuOp = opcode => cpu => cpu.copy(stack = cpu.stack.push(cpu.pc), pc = opcode.NNN)

  val op3XNN: CpuOp = opcode => cpu => cpu.copy(pc = if (cpu.registers.X(opcode)== (opcode.NN)) cpu.pc + 2 else cpu.pc)

  val op4XNN: CpuOp = opcode => cpu => cpu.copy(
    pc =
      if (cpu.registers.X(opcode) != (opcode.NN)) cpu.pc + 2
      else cpu.pc
  )

  val op5XY0: CpuOp = opcode => cpu => cpu.copy(
    pc =
      if (cpu.registers.X(opcode)== cpu.registers.Y(opcode)) cpu.pc + 2
      else cpu.pc
  )

  val op6XNN: CpuOp = opcode => cpu => cpu.copy(registers = cpu.registers.X_(opcode.NN)(opcode))

  val op7XNN: CpuOp = opcode => cpu => cpu.copy(registers = cpu.registers.X_(cpu.registers.X(opcode) + opcode.NN)(opcode))

  val op8XY0: CpuOp = opcode => cpu => cpu.copy(registers = cpu.registers.X_(cpu.registers.Y(opcode))(opcode))

  val op8XY1: CpuOp = opcode => cpu => cpu.copy(registers = cpu.registers.X_(cpu.registers.X(opcode) | cpu.registers.Y(opcode))(opcode))

  val op8XY2: CpuOp = opcode => cpu => cpu.copy(registers = cpu.registers.X_(cpu.registers.X(opcode) & cpu.registers.Y(opcode))(opcode))

  val op8XY3: CpuOp = opcode => cpu => cpu.copy(registers = cpu.registers.X_(cpu.registers.X(opcode) ^ cpu.registers.Y(opcode))(opcode))

  val op8XY4: CpuOp = opcode => cpu => {
    val result = cpu.registers.X(opcode) + cpu.registers.Y(opcode)
    val carryFlag = if (result > 255) 1 else 0
    cpu.copy(
      registers = cpu.registers.X_(Register(result & 0xFF))(opcode)
        .CARRY_(Register(carryFlag))
    )
  }

  val op8XY5: CpuOp = opcode => cpu => {
    val carryFlag = if (cpu.registers.X(opcode) < cpu.registers.Y(opcode)) 0 else 1
    cpu.copy(
      registers = cpu.registers.X_(cpu.registers.X(opcode) - cpu.registers.Y(opcode))(opcode)
        .CARRY_(Register(carryFlag))
    )
  }

  val op8XY6: CpuOp = opcode => cpu => {
    val carryFlag = cpu.registers.X(opcode) & Register(0x1)
    cpu.copy(
      registers = cpu.registers.X_(cpu.registers.X(opcode) >> Register(1))(opcode)
        .CARRY_(carryFlag)
    )
  }

  val op8XY7: CpuOp = opcode => cpu => {
    val carryFlag = if (cpu.registers.Y(opcode) < cpu.registers.X(opcode)) 0 else 1
    cpu.copy(
      registers = cpu.registers.X_(cpu.registers.Y(opcode) - cpu.registers.X(opcode))(opcode)
        .CARRY_(Register(carryFlag))
    )
  }

  val op8XYE: CpuOp = opcode => cpu => {
    val carryFlag = cpu.registers.X(opcode)>> 7
    cpu.copy(
      registers = cpu.registers.X_(Register(cpu.registers.X(opcode) * 2))(opcode)
        .CARRY_(Register(carryFlag))
    )
  }

  val op9XY0: CpuOp = opcode => cpu => cpu.copy(pc = if (cpu.registers.X(opcode)!= cpu.registers.Y(opcode)) cpu.pc + 2 else cpu.pc)

  val opANNN: CpuOp = opcode => _.copy(registerI = Register(opcode.NNN))

  val opBNNN: CpuOp = opcode => cpu => cpu.copy(pc = cpu.registers.registers(0) + opcode.NNN)

  val opCXNN: CpuOp = opcode => cpu => cpu.copy(registers = cpu.registers.X_(Register(Random.nextInt(255) & (opcode.NN)))(opcode))

  val opDXYN: CpuOp = opcode => cpu => {
    val height = opcode & 0x000F
    val coordx = cpu.registers.X(opcode)
    val coordy = cpu.registers.Y(opcode)
    var carryFlag = 0
    var screen = cpu.screen

    for (yline <- 0 until height) {
      val data = cpu.memory.data(cpu.registerI + yline)
      var xpixelinv = 8
      for (xpixel <- 0 until 8) {
        xpixelinv -= 1
        val mask = 1 << xpixelinv
        if ((data & mask) != 0) {
          val x = coordx + xpixel
          val y = coordy + yline
          if ((x < 64) && (y < 32)) {
            if (screen(x)(y) == 1) {
              carryFlag = 1
            }
            screen.update(x, y, screen(x)(y) ^ 1)
          }
        }
      }
    }
    cpu.copy(
      registers = cpu.registers.CARRY_(Register(carryFlag)),
      screen = screen
    )
  }

  val opEX9E: CpuOp = _ => identity

  val opEXA1: CpuOp = opcode => cpu => cpu.copy(pc = cpu.pc + 2)

  val opFX07: CpuOp = opcode => cpu => cpu.copy(registers = cpu.registers.X_(Register(cpu.delayTimer))(opcode))

  val opFX0A: CpuOp = _ => identity

  val opFX15: CpuOp = opcode => cpu => cpu.copy(delayTimer = cpu.registers.X(opcode))

  val opFX18: CpuOp = opcode => cpu => cpu.copy(soundTimer = cpu.registers.X(opcode))

  val opFX1E: CpuOp = opcode => cpu => cpu.copy(registerI = cpu.registers.X(opcode) + cpu.registerI)

  val opFX29: CpuOp = opcode => cpu => cpu.copy(registerI = cpu.registers.X(opcode) * Register(5))

  val opFX33: CpuOp = opcode => cpu => {
    val updatedMemory = Memory(cpu.memory.data
      .updated(cpu.registerI + 0, cpu.registers.X(opcode) / 100)
      .updated(cpu.registerI + 1, (cpu.registers.X(opcode) / 10) % 10)
      .updated(cpu.registerI + 2, cpu.registers.X(opcode) % 10))
    cpu.copy(memory = updatedMemory)
  }

  val opFX55: CpuOp = opcode => cpu => {
    val x = (opcode & 0x0F00) >> 8
    val updatedMemory = (0 to x).zipWithIndex.foldLeft(cpu.memory.data){
      case (mem, (value, index)) => mem.updated(cpu.registerI + index, cpu.registers.registers(index))
    }
    cpu.copy(memory = Memory(updatedMemory))
  }

  val opFX65: CpuOp = opcode => cpu => {
    val x = (opcode & 0x0F00) >> 8
    val updatedRegisters = (0 to x).zipWithIndex.foldLeft(cpu.registers.registers){
      case (regs, (value, index)) => {
        regs.updated(index, Register(cpu.memory.data(cpu.registerI + index)))
      }
    }
    cpu.copy(registers = Registers(updatedRegisters))
  }
}