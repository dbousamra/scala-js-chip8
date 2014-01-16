package example

import scala.scalajs.js
import js.Dynamic.{ global => g }
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}
import org.scalajs.jquery._
import chip8.{Roms, Memory, Cpu, Screen}

object ScalaJSExample {

  def main(): Unit = {
    val playground = jQuery("#playground")
    val width  = 12 * 64
    val height = 12 * 34
    val canvas = jQuery("<canvas width='" + width + "' height='" + height + "'></canvas>")
    val domCanvas = canvas.get(0).asInstanceOf[HTMLCanvasElement]
    val context = domCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    playground.append(jQuery("<div>").append(canvas))

    var cpu = Cpu(memory = Memory.fromData(Roms.LOGO))
    for (i <- 0 until 400) {
      cpu = cpu.emulate(drawScreen(context))(cpu)
    }
  }

  def drawScreen(context: CanvasRenderingContext2D)(cpu: Cpu): Cpu =  {
    context.fillStyle = "green"
    g.console.log("IN draw screen mofo")
    for (x <- 0 until 64) {
      for (y <- 0 until 32) {
        if (cpu.screen(x)(y) == 1) {
          g.setTimeout()
          context.fillRect(x * 12, y * 12, 12, 12)
        }
      }
    }
    cpu
  }
}
