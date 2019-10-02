package simprocessing

import data.HighVelocityCollisions
import java.io.File
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.ColorGradient
import util.OrbitSpeed

object HighVelsAnnulusPlot {
  def main(args: Array[String]): Unit = {
    if (args.length < 5) {
      println("Required arguments:")
      println("  High Velocity File")
      println("  x multiplier")
      println("  speed-size multiplier")
      println("  startStep")
      println("  max to take")
      sys.exit(0)
    }
    val file = new File(args(0))
    val xMult = args(1).toDouble
    val speedMult = args(2).toDouble
    val startStep = args(3).toInt
    val maxToTake = args(4).toInt
    val data = HighVelocityCollisions.read(file).filter(_.step >= startStep)
    println("Last step is "+data.last.step)
    val allVels = data.flatMap(step => step.colls.map(_.vel)).sorted.takeRight(10)
    println(s"Highest vels = ${allVels.map(v => OrbitSpeed(5e18, 391/(2.0/(3.0*4)+1), v))}")
    val cxs = data.flatMap(step => step.colls.take(maxToTake).reverse.map(c => 0.5*(c.p1.x+c.p2.x)))
    val cys = data.flatMap(step => step.colls.take(maxToTake).reverse.map(c => 0.5*(c.p1.y+c.p2.y)))
    val xs = (cxs, cys).zipped.map((x, y) => math.cos(y)*(1+x*xMult))
    val ys = (cxs, cys).zipped.map((x, y) => math.sin(y)*(1+x*xMult))
    val sizes = data.flatMap(step => step.colls.take(maxToTake).reverse.map(c => c.vel * speedMult))
    val min = sizes.min
    val max = sizes.max
    val cg = ColorGradient(min -> BlackARGB, min+0.3333*(max-min) -> BlueARGB, min+0.6666*(max-min) -> GreenARGB, max -> RedARGB)
    println("Num points: "+sizes.length)
    val plot = Plot.simple(ScatterStyle(xs, ys, symbolWidth = sizes, symbolHeight = sizes, colors = cg(sizes)))
    SwingRenderer(plot, 1000, 1000, true)
  }
}