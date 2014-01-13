package chip8

import java.io.File

object Emulator {

  def main(args: Array[String]) {
    val cpu = Cpu(memory = Memory.fromFile(new File("src/resources/roms/TETRIS")))
    cpu.run()
  }
}
