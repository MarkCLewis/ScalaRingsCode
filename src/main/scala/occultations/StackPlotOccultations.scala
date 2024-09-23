package occultations

import collection.mutable
import occultations.PlotOccultation.DataLine
import swiftvis2.plotting._
import swiftvis2.plotting.styles._
import swiftvis2.plotting.styles.ScatterStyle.LineData
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.renderer.SwingRenderer


object StackPlotOccultations {
  def main(args: Array[String]): Unit = {
    val data = args.tail.map(file => PlotOccultation.readOccultationFile(file)._1.toArray)
    val plot = createStackPlot(data, args(0).toDouble)
    SwingRenderer(plot, 1000, 1000, true)
  }

  def createStackPlot(occultations: Array[Array[DataLine]], R0: Double, range: Option[(Double, Double)] = None, xOffset: Double = 0.0): Plot = {
    val scatters = for (occultationData <- occultations) yield {
      ScatterStyle(occultationData.map(od => ((od.startX + od.endX)*0.5+xOffset)*R0), occultationData.map(_.trans), symbol = NoSymbol, 
        lines = Some(LineData(0, stroke = Renderer.StrokeData(1.0))))
    }
    val plot = scatters.indices.foldLeft(Plot.gridNN(scatters.toSeq.map(s => Seq(s)), "", "Radial", "Photon Count")) { case (p, i) =>
        p.withModifiedAxis[NumericAxis]("y", s"y$i", a => a.copy()).updatedYAxisOnPlot(i, 0, 0, s"y$i")
    }.updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.3g"))
    range.fold(plot){ case (min, max) =>
      plot.updatedAxis[NumericAxis]("x", a => a.copy(min = Some(min), max = Some(max)))
    }
  }
  
}