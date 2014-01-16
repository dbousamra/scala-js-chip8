package chip8
import scala.collection.immutable.Stack

case class Cpu(
  val pc: Int = 0x200,
  val memory: Memory,
  val stack: Stack[Int] = Stack(),
  val delayTimer: Int = 0,
  val soundTimer: Int = 0,
  val screen: Screen = Screen(65, 33),
  val registers: Registers = Registers(Vector.fill[Register](16)(0)),
  val registerI: Register = 0) {

  def nextOpcode = memory.data(pc) << 8 | memory.data(pc + 1)

  def handleOpcode(beforeExecution: Cpu): Cpu = {
    val opcodeFunction = Opcodes.fetch(beforeExecution.nextOpcode)
    opcodeFunction(beforeExecution.copy(beforeExecution.pc + 2))
  }

  def handleTimers: CpuReader = cpu => cpu.copy(
  delayTimer = if (cpu.delayTimer > 0) cpu.delayTimer - 1 else cpu.delayTimer,
  soundTimer = if (cpu.soundTimer > 0) cpu.soundTimer - 1 else cpu.soundTimer
  )

  def handleInput: CpuReader = identity

  def emulate(drawScreen: CpuReader) = {
    handleOpcode _ andThen
    handleTimers   andThen
    handleInput    andThen
    drawScreen
  }
}

case class Screen(x: Int, y: Int) {
  val s = Array.ofDim[Int](x, y)

  def apply(x: Int) = s(x)

  def update(x: Int, y: Int, value: Int) = s(x)(y) = value
}

