package chip8

import java.io.File

object Emulator {

  def main(args: Array[String]) {
    val memory = Memory.fromFile(new File("src/resources/roms/TETRIS"))
    val cpu = Cpu(memory = memory)
    cpu.run()
  }
}
