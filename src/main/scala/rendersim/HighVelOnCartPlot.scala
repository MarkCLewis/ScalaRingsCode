package rendersim

import data.CartAndRad
import data.HighVelocityCollisions
import java.io.File
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.JVMSVGInterface

object HighVelOnCartPlot extends App {
  if (args.length < 4) {
    println("Args: dir cartStep stepsBack r0")
    sys.exit()
  }
  val dir = new File(args(0))
  val cartStep = args(1).toInt
  val stepsBack = args(2).toInt
  val r0 = args(3).toDouble
  val labelFormat = "%1.1f"
  val collSteps = HighVelocityCollisions.read(new File(dir, "HighVelColls.bin"))
  val cg = ColorGradient(0.0 -> BlackARGB, 2e-6 -> GreenARGB, 4e-6 -> RedARGB)
  val steps = collSteps.filter(cs => cs.step >= cartStep - stepsBack && cs.step <= cartStep)
  val carFile = new File(dir, "CartAndRad." + cartStep + ".bin")
  // TODO: Make it filter around the moon and have the sizes of the particles scaled. Requires always reading the file to filter.
  if (carFile.exists) {
    val particles = CartAndRad.read(carFile)
    val moonlet = particles.last
    val plotColls = steps.flatMap(_.colls.filter(cd => ((cd.p1.y + cd.p2.y) * 0.5 - moonlet.y).abs < 1e-5))
    println("Colls near moonlet = " + plotColls.length)
    val plotParts = particles.filter(p => (p.y - moonlet.y).abs < 1e-5)
    val collStyle = ScatterStyle(plotColls.map(cd => (cd.p1.x + cd.p2.x) * 0.5 * r0), plotColls.map(cd => (cd.p1.y + cd.p2.y) * 0.5 * r0),
      symbolWidth = 5, symbolHeight = 5, colors = cg(plotColls.map(_.vel)))
    val carStyle = ScatterStyle(plotParts.map(_.x * r0), plotParts.map(_.y * r0), symbolWidth = plotParts.map(_.rad * 2 * r0),
      symbolHeight = plotParts.map(_.rad * 2 * r0), xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
    val plot = Plot.stacked(List(carStyle, collStyle))
      .updatedAxis[NumericAxis]("x", na => na.numberFormat(labelFormat).updatedName("Radial [km]"))
      .updatedAxis[NumericAxis]("y", na => na.numberFormat(labelFormat).updatedName("Azimuthal [km]"))
    val num = cartStep.toString()
    SwingRenderer.saveToImage(plot, s"plotHighVelCart.${"0"*(5-num.length)+num}.png", width = 1600, height = 1600)
    JVMSVGInterface.apply(plot, s"plotHighVelCart.${"0"*(5-num.length)+num}.svg", 1600, 1600)
  }
}