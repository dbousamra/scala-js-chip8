package example

import scala.scalajs.js
import js.Dynamic.{ global => g }
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement}
import org.scalajs.jquery._
import chip8.{Memory, Cpu}

object ScalaJSExample {

  val playground = jQuery("#playground")
  val width  = 12 * 64
  val height = 12 * 34
  val canvas = jQuery("<canvas width='" + width + "' height='" + height + "'></canvas>")
  val domCanvas = canvas.get(0).asInstanceOf[HTMLCanvasElement]
  val context = domCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

  var cpu: Cpu = Cpu(memory = Memory.fromData(getFile("src/resources/roms/LOGO")))

  def main(): Unit = {
    playground.append(jQuery("<div>").append(canvas))
    createGameSelector
    g.window.requestAnimationFrame(run)
  }

  def createGameSelector = jQuery("#games").on("change", {(event: JQueryEventObject) => {
    val game = jQuery(event).`val`().asInstanceOf[String]
    cpu = Cpu(memory = Memory.fromData(getFile("src/resources/roms/" + game)))
  }}: js.ThisFunction)

  def run: js.Function = () => {
    cpu = cpu.emulate(20)(render(domCanvas, context))(cpu)
    g.window.requestAnimationFrame(run)
  }

  def getFile(romUrl: String): Array[Int] = {
    val xhr = js.Dynamic.newInstance(g.XMLHttpRequest)()
    xhr.open("GET", romUrl , false)
    xhr.overrideMimeType("text/plain; charset=x-user-defined");
    xhr.send(null);
    val response = xhr.response.asInstanceOf[String]
    response.toArray.map(_.toInt & 0xFF)
  }
  def clearScreen(canvas: HTMLCanvasElement, context: CanvasRenderingContext2D) = {
    context.clearRect(0, 0, canvas.width, canvas.height);
  }

  def render(canvas: HTMLCanvasElement, context: CanvasRenderingContext2D)(cpu: Cpu): Cpu =  {
    if (cpu.screen.drawFlag) {
      clearScreen(canvas, context)
      for (x <- 0 until 64; y <- 0 until 32) {
        if (cpu.screen(x)(y) == 1) {
          context.fillStyle   = "#FF4136"
          context.fillRect(x * 12, y * 12, 12, 12)
        }
      }
    }
    cpu.copy(screen = cpu.screen.copy(drawFlag = false)(cpu.screen.data))
  }
}
