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
    implicit val domCanvas = canvas.get(0).asInstanceOf[HTMLCanvasElement]
    val context = domCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    playground.append(jQuery("<div>").append(canvas))

    var cpu = Cpu(memory = Memory.fromData(Roms.TANK))
//    for (i <- 0 until 10) {
//      for (j <- 0 until 1000) {
//        cpu = cpu.emulate(render(domCanvas, context))(cpu)
//      }
//      cpu = render(domCanvas, context)(cpu)
//    }

//    g.jQuery("h1").each({ (item: js.Any, index : js.Number) =>
//      g.console.log(s"$index: ${g.jQuery(item).text()}")
//    }: js.ThisFunction)

//    g.window.requestAnimationFrame()
    println("hello")
  }

  def clearScreen(canvas: HTMLCanvasElement, context: CanvasRenderingContext2D) = {
    context.clearRect(0, 0, canvas.width, canvas.height);
  }

  def render(canvas: HTMLCanvasElement, context: CanvasRenderingContext2D)(cpu: Cpu): Cpu =  {
    if (cpu.screen.drawFlag) {
      clearScreen(canvas, context)
      println("CALLED")
      for (x <- 0 until 64; y <- 0 until 32) {
        if (cpu.screen(x)(y) == 1) {
//          g.setTimeout(10)
          context.fillRect(x * 12, y * 12, 12, 12)
        }
      }
    }
    cpu.copy(screen = cpu.screen.copy(drawFlag = false)(cpu.screen.data))
  }
}
