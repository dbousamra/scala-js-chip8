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

  def Register(i: Int): Register = i

  def debug(cpu: Cpu) = {
    println("PC before = " + cpu.pc.toHexString +
      " opcode before = " + cpu.nextOpcode.toHexString +
      " instruction before = " + (cpu.nextOpcode & 0xF000).toHexString)
  }
}
