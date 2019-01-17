package rendersim

import data.CartAndRad
import data.HighVelocityCollisions
import java.io.File
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.styles.ScatterStyle

object TestCollisionReading extends App {
  val dir = new File("/data/mlewis/Rings/AMNS-Moonlets/HighRes/Moonlet4")
  val collSteps = HighVelocityCollisions.read(new File(dir, "HighVelColls.bin"))
  val cg = ColorGradient(0.0 -> BlackARGB, 5e-7 -> BlueARGB, 1e-6 -> GreenARGB, 2e-6 -> RedARGB)
  val updater1 = SwingRenderer(Plot.scatterPlot(1 to 10, 1 to 10), 1000, 1000, true)
  val updater2 = SwingRenderer(Plot.scatterPlot(1 to 10, 1 to 10), 1000, 1000, true)
  for (step <- collSteps.takeRight(5)) {
    println(step.step + " " + step.colls.last.vel)
    val carFile = new File(dir, "CartAndRad." + step.step + ".bin")
    // TODO: Make it filter around the moon and have the sizes of the particles scaled. Requires always reading the file to filter.
    if (carFile.exists) {
      val particles = CartAndRad.read(carFile)
      val moonlet = particles.last
      val plotColls = step.colls.filter(cd => ((cd.p1.y + cd.p2.y) * 0.5 - moonlet.y).abs < 1e-5)
      println("Colls near moonlet = " + plotColls.length)
      val plotParts = particles.filter(p => (p.y - moonlet.y).abs < 1e-5)
      val collStyle = ScatterStyle(plotColls.map(cd => (cd.p1.x + cd.p2.x) * 0.5 * 1.3e5), plotColls.map(cd => (cd.p1.y + cd.p2.y) * 0.5 * 1.3e5),
        symbolWidth = 5, symbolHeight = 5, colors = cg(plotColls.map(_.vel)))
      val carStyle = ScatterStyle(plotParts.map(_.x * 1.3e5), plotParts.map(_.y * 1.3e5), symbolWidth = plotParts.map(_.rad * 2 * 1.3e5),
        symbolHeight = plotParts.map(_.rad * 2 * 1.3e5), xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
      val plot = Plot.stacked(List(carStyle, collStyle), title = step.step + " " + step.colls.length)
      updater1.update(plot)
      updater2.update(Plot.simple(ScatterStyle(step.colls.map(cd => (cd.p1.x + cd.p2.x) * 0.5 * 1.3e5), step.colls.map(cd => (cd.p1.y + cd.p2.y) * 0.5 * 1.3e5),
        symbolWidth = 5, symbolHeight = 5, colors = cg(plotColls.map(_.vel)))))
    }
  }
}