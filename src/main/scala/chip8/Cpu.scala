package chip8

import scala.collection.immutable.Stack

case class Cpu(
  val pc: Int = 0x200,
  val memory: Memory,
  val stack: Stack[Int] = Stack(),
  val registers: List[Register] = List.fill[Register](16)(new Register(0))
)


