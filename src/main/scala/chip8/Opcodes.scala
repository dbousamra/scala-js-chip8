package chip8

object Opcodes {

  def fetch(implicit opcode: Int): Cpu => Cpu = opcode & 0xF000 match {
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
      //      case 0x4 => op8XY4
      //      case 0x5 => op8XY5
      //      case 0x6 => op8XY6
      //      case 0x7 => op8XY7
      //      case 0xE => op8XYE
    }
    case 0xA000 => opANNN
//    case _ => throw new NotImplementedError()
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

  def op3XNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc =
      if (cpu.registers.X.value  == (opcode & 0x00FF)) cpu.pc + 2
      else cpu.pc
  )

  def op4XNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc =
      if (cpu.registers.X.value  != (opcode & 0x00FF)) cpu.pc + 2
      else cpu.pc
  )

  def op5XY0(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    pc =
      if (cpu.registers.X == cpu.registers.Y) cpu.pc + 2
      else cpu.pc
  )

  def op6XNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(Register(opcode & 0x00FF))
  )

  def op7XNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(Register(cpu.registers.X.value + opcode & 0x00FF))
  )

  def op8XY0(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.Y)
  )

  def op8XY1(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.X | cpu.registers.Y)
  )


  def op8XY2(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.X & cpu.registers.Y)
  )

  def op8XY3(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registers = cpu.registers.X_(cpu.registers.X ^ cpu.registers.Y)
  )

  // TODO possibly wrong
  def op8XY4(cpu: Cpu)(implicit opcode: Int) = {
    val result = cpu.registers.X + cpu.registers.Y
    val carryFlag = if (result.value > 255) 1 else 0
    cpu.copy(
      registers = cpu.registers.X_(Register(result.value & 0xFF)).CARRY_(Register(carryFlag))
    )
  }



  def opANNN(cpu: Cpu)(implicit opcode: Int) = cpu.copy(
    registerI = Register(opcode & 0x0FFF)
  )
}