package photometry

import data.CartAndRad
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import java.io.File
import util.GCCoord

object FigureOutDust extends App {
  val labelFormat = "%1.1e"
  val FileRegex = """.*CartAndRad.(\d+).bin""".r
  val dataDir = new File("/home/mlewis/workspaceResearch/RegolithImpactSims/ImpactTesting/Reg/")
  for (simDir <- dataDir.listFiles(); if simDir.isDirectory() && new File(simDir, "CartAndRad.0.bin").exists()) {
    val imageDir = new File(simDir, "plots")
    imageDir.mkdirs()
    val dataFiles = simDir.listFiles().filter(_.getName().startsWith("CartAndRad"))
    val data0 = CartAndRad.read(new File(simDir, "CartAndRad.0.bin"))
    val rad0 = data0.map(_.rad * 2)
    val scatter0 = ScatterStyle(
      data0.map(_.x),
      data0.map(_.y),
      EllipseLine,
      rad0,
      rad0,
      PlotSymbol.Sizing.Scaled,
      PlotSymbol.Sizing.Scaled,
      GreenARGB
    )
    val vector0 = ScatterStyle(
      data0.filter(_.rad > 5e-9).flatMap(p => Array(p.x, p.x + p.vx)),
      data0.filter(_.rad > 5e-9).flatMap(p => Array(p.y, p.y + p.vy)),
      NoSymbol,
      colors = GreenARGB,
      lines = Some(ScatterStyle.LineData(data0.indices.flatMap(i => Array(i, i)): PlotIntSeries))
    )
    for (file <- dataFiles; if FileRegex.findFirstIn(file.getName()).nonEmpty) {
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
      val plot = Plot.stackedGridNN(Seq(Seq(
        Seq(
          scatter0,
          vector0,
          ScatterStyle(
            data.map(_.x),
            data.map(_.y),
            symbolWidth = rads, 
            symbolHeight = rads,
          )
        ),
        Seq(
          ScatterStyle(
            (data, gcdata).zipped.flatMap((c, gc) => Array(c.x, gc.X)),
            (data, gcdata).zipped.flatMap((c, gc) => Array(c.y, gc.Y)),
            NoSymbol,
            colors = phiGrad(gcdata.flatMap(gc => Array(gc.phi, gc.phi))),
            lines = Some(ScatterStyle.LineData(data0.indices.flatMap(i => Array(i, i)): PlotIntSeries))
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
      )))
      .updatedAxis[NumericAxis]("x", na => na.numberFormat(labelFormat).updatedName("Radial"))
      .updatedAxis[NumericAxis]("y", na => na.numberFormat(labelFormat).updatedName("Azimuthal"))
      .withText("label", PlotText(simDir.getName()), Bounds(0.5, 0.05, 0.4, 0.05))
      // SwingRenderer.saveToImage(
      //   plot,
      //   "output." + ("0" * (7 - fnum.length)) + fnum + ".png",
      //   width = 1200,
      //   height = 1200
      // )
      val FileRegex(num) = file.getName()
      SwingRenderer.saveToImage(plot, s"${imageDir.getAbsolutePath()}/plotCart.${"0"*(5-num.length)+num}.png", width = 1600, height = 800)
    }
  }

}
