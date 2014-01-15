package example

import scala.scalajs.js
import js.Dynamic.{ global => g }
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}
import org.scalajs.jquery._
import chip8.{Roms, Memory, Cpu, Screen}

object ScalaJSExample {

  def main(): Unit = {
    val playground = jQuery("#playground")
    val width  = 196
    val height = 128
    val canvas = jQuery("<canvas width='" + width + "' height='" + height + "'></canvas>")
    val domCanvas = canvas.get(0).asInstanceOf[HTMLCanvasElement]
    val context = domCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    playground.append(jQuery("<div>").append(canvas))

    var cpu = Cpu(memory = Memory.fromData(Roms.TANK))
    for (i <- 0 until 400) {
      cpu = cpu.emulate(drawScreen(context))(cpu)
    }
  }

  def drawScreen(context: CanvasRenderingContext2D)(cpu: Cpu): Cpu =  {
//    val SquareSizePx = 12
//    val x: js.Number = square.x
//    val y: js.Number = square.y
//
//    // Background
//    context.fillStyle = "green"
//    context.fillRect(x, y, SquareSizePx, SquareSizePx)
    g.console.log("IN draw screen mofo")
    cpu
  }
}
