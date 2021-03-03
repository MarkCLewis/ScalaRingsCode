package photometry

import data.CartAndRad
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import java.io.File

object FigureOutDust extends App {
  val labelFormat = "%1.3e"
  val FileRegex = """.*CartAndRad.(\d+).bin""".r
  val cartStep = "CartAndRad.1000.bin"
  val dataDir = new File("/home/mlewis/workspaceResearch/RegolithImpactSims/ImpactTesting/Reg/")
  for (simDir <- dataDir.listFiles()) {
    val file = new File(simDir, cartStep)
    if (file.exists()) {
      val data = CartAndRad.read(file)
      val rads = data.map(_.rad)
      val plot = Plot.simple(
        ScatterStyle(
          data.map(_.x),
          data.map(_.y),
          symbolWidth = rads, 
          symbolHeight = rads,
          xSizing = PlotSymbol.Sizing.Scaled,
          ySizing = PlotSymbol.Sizing.Scaled
        )
      ) //.
      //updatedAxis[NumericAxis]("x", na => na.copy(min = xaxisMin, max = xaxisMax, tickLabelInfo = na.tickLabelInfo.map(_.copy(numberFormat = labelFormat)))).
      //updatedAxis[NumericAxis]("y", na => na.copy(min = yaxisMin, max = yaxisMax, tickLabelInfo = na.tickLabelInfo.map(_.copy(numberFormat = labelFormat))))
      // SwingRenderer.saveToImage(
      //   plot,
      //   "output." + ("0" * (7 - fnum.length)) + fnum + ".png",
      //   width = 1200,
      //   height = 1200
      // )
      SwingRenderer(plot, 600, 600, true)
    }
  }

}
