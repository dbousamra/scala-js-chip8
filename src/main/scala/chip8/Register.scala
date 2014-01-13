package chip8


//TODO do we need a register class
case class Register(value: Int) {
  def ==(i: Register) = value == i.value
  def +(i: Register) = new Register(value + i.value)
  def *(i: Register) = new Register(value * i.value)
  def -(i: Register) = new Register(value - i.value)
  def <<(i: Register) = new Register(value << i.value)
  def >>(i: Register) = new Register(value >> i.value)
  def &(i: Register) = new Register(value & i.value)
  def |(i: Register) = new Register(value | i.value)
  def ^(i: Register) = new Register(value ^ i.value)
}

case class Registers(val registers: List[Register]) {
  def X(implicit address: Int) = registers((address & 0x0F00) >> 8)
  def Y(implicit address: Int) = registers((address & 0x00F0) >> 4)
  def CARRY = registers(0xF)

  def X_(value: Register)(implicit address: Int): Registers = {
    Registers(registers.updated((address & 0x0F00) >> 8, value))
  }

  def Y_(value: Register)(implicit address: Int): Registers = {
    Registers(registers.updated((address & 0x0F00) >> 8, value))
  }

  def CARRY_(value: Register): Registers = {
    Registers(registers.updated(0xF, value))
  }
}
