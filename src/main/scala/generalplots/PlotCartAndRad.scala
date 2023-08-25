package generalplots

import data.CartAndRad
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.styles.ScatterStyle

object PlotCartAndRad {
  def main(args: Array[String]): Unit = {
    val data = CartAndRad.read(new java.io.File(args(0))).sortBy(_.z)
    val diams = data.map(_.rad * 2)
    val thickness = if (args.length > 1) args(1).toDouble else 2e-8
    val gradient = ColorGradient(-thickness -> BlueARGB, -thickness/2 -> CyanARGB, 0.0 -> GreenARGB, thickness/2 -> YellowARGB, thickness -> RedARGB)
    val scatter = ScatterStyle(data.map(_.x), data.map(_.y), symbolWidth = diams, symbolHeight = diams, colors = gradient(data.map(_.z)), xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
    val plot = Plot.simple(scatter, "", "Radial", "Azimuthal")
      .updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.2g"))
      .updatedAxis[NumericAxis]("y", a => a.updatedNumberFormat("%1.2g"))
    SwingRenderer(plot, 1200, 1200, true)
  }
}
