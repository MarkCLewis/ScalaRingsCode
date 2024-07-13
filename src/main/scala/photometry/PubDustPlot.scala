package photometry

import data.CartAndRad
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import java.io.File
import util.GCCoord
import swiftvis2.plotting.renderer.SVGRenderer
import swiftvis2.plotting.renderer.JVMSVGInterface

/**
 * This is a version of figure out dust that is intended to make a nicer plot for publication purposes.
 * Specify a directory followed by all the steps you want to plot.
 */
object PubDustPlot extends App {
  val labelFormat = "%1.1e"
  val simDir = new File(args(0))
  val imageDir = new File(simDir, "plots")
  imageDir.mkdirs()
  val rows = for (num <- args.drop(1).toIndexedSeq; file = new File(simDir, s"CartAndRad.$num.bin"); if file.exists) yield {
    val data = CartAndRad.read(file)
    val gcdata = data.map(p => GCCoord(p))
    val rads = data.map(p => if (p.rad > 5e-9) 6 else 2)
    val phiGrad = ColorGradient(
      -math.Pi -> RedARGB, 
      -2*math.Pi/3 -> YellowARGB,
      -math.Pi -> GreenARGB,
      0.0 -> CyanARGB,
      math.Pi/3 -> BlueARGB,
      2*math.Pi/3 -> MagentaARGB,
      math.Pi -> RedARGB
    )
    Seq(
      Seq(
        ScatterStyle(
          data.lazyZip(gcdata).flatMap((c, gc) => Array(c.x, gc.X)),
          data.lazyZip(gcdata).flatMap((c, gc) => Array(c.y, gc.Y)),
          NoSymbol,
          colors = phiGrad(gcdata.flatMap(gc => Array(gc.phi, gc.phi))),
          lines = Some(ScatterStyle.LineData(data.indices.flatMap(i => Array(i, i)): PlotIntSeries))
        ),
        ScatterStyle(
          gcdata.map(_.X),
          gcdata.map(_.Y),
          symbolWidth = rads, 
          symbolHeight = rads,
          colors = RedARGB,
        ),
        ScatterStyle(
          data.map(_.x),
          data.map(_.y),
          symbolWidth = data.map(_.rad * 2), 
          symbolHeight = data.map(_.rad * 2),
          xSizing = PlotSymbol.Sizing.Scaled,
          ySizing = PlotSymbol.Sizing.Scaled,
        )
      )
    )
  }
  val plot = Plot.stackedGridNN(rows)
    .updatedAxis[NumericAxis]("x", na => na.numberFormat(labelFormat).updatedName("Radial"))
    .updatedAxis[NumericAxis]("y", na => na.numberFormat(labelFormat).updatedName("Azimuthal"))
    // .withText("label", PlotText(simDir.getName()), Bounds(0.5, 0.05, 0.4, 0.05))
  SwingRenderer.saveToImage(plot, s"${imageDir.getAbsolutePath()}/plotCart.pub.png", width = 1200, height = 1200)
  JVMSVGInterface.apply(plot, s"${imageDir.getAbsolutePath()}/plotCart.pub.svg", 1200, 1200)

}