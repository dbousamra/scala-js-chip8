package chip8

import scala.collection.immutable.Stack

case class Cpu(
  val pc: Int = 0x200,
  val memory: Memory,
  val stack: Stack[Int] = Stack(),
  val registers: Registers = Registers(List.fill[Register](16)(new Register(0))),
  val registerI: Register = Register(0)) {

  private def execute(beforeExecution: Cpu): Cpu = {
    debug(beforeExecution)
    val opcodeFunction = Opcodes.fetch(beforeExecution.nextOpcode)
    opcodeFunction(beforeExecution.copy(pc + 2))
  }

  private val nextOpcode = memory.data(pc) << 8 | memory.data(pc + 1)

  private def debug(cpu: Cpu) = {
    println("PC before = " + cpu.pc.toHexString +
      " opcode before = " + cpu.nextOpcode.toHexString +
      " instruction before = " + (cpu.nextOpcode & 0xF000).toHexString)
  }

  def run(cpu: Cpu = this): Cpu = run(cpu.execute(cpu))
}

