package chip8

object Opcodes {

  def fetch(implicit opcode: Int): Cpu => Cpu = opcode match {
    case 0x0000 => opcode & 0xF match {
      case 0x0000 => clearScreen
      case 0x000E => op00EE
    }
    case 0x1000 => op1NNN
    case 0x2000 => op2NNN
    case 0x3000 => op3XNN
//    case 0x4000 => op4XNN
//    case 0x5000 => op5XY0
//    case 0x6000 => op6XNN
//    case 0x7000 => op7XNN
  }

  def clearScreen(cpu: Cpu)(implicit opcode: Int) = {
    cpu
  }

  def op00EE(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc = cpu.stack.head,
    stack = cpu.stack.pop
  )

  def op1NNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc = opcode & 0x0FFF
  )

  def op2NNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    stack = cpu.stack.push(cpu.pc),
    pc = opcode & 0x0FFF
  )

  def op3XNN(cpu: Cpu)(implicit opcode: Int) = {
    val register = cpu.registers((opcode & 0x0F00) >> 8)
    cpu.copy(
      pc =
        if (register.value  == (opcode & 0x00FF)) cpu.pc + 2
        else cpu.pc
    )
  }

  def op4XNN(cpu: Cpu)(implicit opcode: Int) = {
    val register = cpu.registers((opcode & 0x0F00) >> 8)
    cpu.copy(
      pc =
        if (register.value  != (opcode & 0x00FF)) cpu.pc + 2
        else cpu.pc
    )
  }

  def op5XY0(cpu: Cpu)(implicit opcode: Int) = {
    val register1 = cpu.registers((opcode & 0x0F00) >> 8)
    val register2 = cpu.registers((opcode & 0x00F0) >> 4)
    cpu.copy(
      pc =
        if (register1 == register2) cpu.pc + 2
        else cpu.pc
    )
  }

  def op6XKK(cpu: Cpu)(implicit opcode: Int) = {
    val value = opcode & 0x00FF
    cpu.copy(
      registers = cpu.registers.updated((opcode & 0x0F00) >> 8, Register(value))
    )
  }
}