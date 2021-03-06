/**
 * Created with IntelliJ IDEA.
 * User: domlebo70
 * Date: 16/01/2014
 * Time: 8:25 AM
 * To change this template use File | Settings | File Templates.
 */
package object chip8 {

  type Opcode = Int
  type Register = Int
  type Address = Int
  type CpuReader = Cpu => Cpu
  type CpuOp = Opcode => Cpu => Cpu

  implicit class RichOpcode(val opcode: Opcode) extends AnyVal {
    def instruction = opcode & 0xF000
    def NNN = opcode & 0x0FFF
    def NN = opcode & 0x00FF
    def N =  opcode & 0x000F
  }

  implicit class nTimes(val n: Int) extends AnyVal {
    def times[T](f: => T => T) = List.fill(n)(f).foldRight(identity: T => T){ (x, y) => y.andThen(x) }
  }

  def Register(i: Int): Register = i

  def debug: CpuReader = cpu => {
    println("PC before = " + cpu.pc.toHexString +
      " opcode before = " + cpu.nextOpcode.toHexString +
      " instruction before = " + (cpu.nextOpcode & 0xF000).toHexString +
      " registers = " + cpu.registers.registers.map(_.toHexString))
    cpu
  }
}
