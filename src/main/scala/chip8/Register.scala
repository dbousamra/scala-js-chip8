package chip8

case class Register(value: Int) {
  def ==(i: Register) = value == i.value
  def +(i: Register) = new Register(value + i.value)
  def -(i: Register) = new Register(value - i.value)
  def <<(i: Register) = new Register(value << i.value)
  def >>(i: Register) = new Register(value >> i.value)
  def &(i: Register) = new Register(value & i.value)
  def |(i: Register) = new Register(value | i.value)
  def ^(i: Register) = new Register(value ^ i.value)
}
