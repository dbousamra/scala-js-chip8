package chip8

case class Registers(val registers: IndexedSeq[Register]) {

  def X(implicit address: Address) = registers((address & 0x0F00) >> 8)
  def Y(implicit address: Address) = registers((address & 0x00F0) >> 4)
  def CARRY = registers(0xF)

  def X_(value: Register)(implicit address: Address): Registers = {
    Registers(registers.updated((address & 0x0F00) >> 8, value & 0xFF))
  }

  def Y_(value: Register)(implicit address: Address): Registers = {
    Registers(registers.updated((address & 0x0F00) >> 8, value & 0xFF))
  }

  def CARRY_(value: Register): Registers = {
    Registers(registers.updated(0xF, value & 0xFF))
  }
}