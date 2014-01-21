package chip8


object Main {
  def main(args: Array[String]) {
    var cpu = Cpu(memory = Memory.fromData(Roms.LOGO))
    for (i <- 0 until 400) {
      cpu = cpu.emulate(cpu => cpu)(cpu)
    }
  }
}
