package occultations

import javafx.scene.chart.PieChart.Data
import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer
import scalafx.application.ConditionalFeature

object PlotOccultation {
  val lineRegex =
    """MeasurementDetails\((.*)\)|CartAndRad\.(\d+)\.bin|Index.*|(\d+)\s+(\d+)\s+(\d+)\s+(.+)\s+(.+)\s+(.+)\s+(.+)\s+(.+)""".r

  case class DataLine(
      index: Int,
      photon: Int,
      trans: Int,
      fraction: Double,
      startX: Double,
      startY: Double,
      endX: Double,
      endY: Double
  )

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile(args(0))
    val lines = source.getLines()
    var details = ""
    var step = ""
    val data = collection.mutable.Buffer[DataLine]()
    for (lineRegex(
           md,
           s,
           index,
           photon,
           trans,
           fraction,
           startX,
           startY,
           endX,
           endY
         ) <- lines) {

      if (md != null) {
        details = md
        if (data.nonEmpty) {
          plot(details, step, data.toArray)
        }
      } else if (step != null) {
        step = s
      } else if (index != null) {
        data += DataLine(
          index.toInt,
          photon.toInt,
          trans.toInt,
          fraction.toDouble,
          startX.toDouble,
          startY.toDouble,
          endX.toDouble,
          endY.toDouble
        )

      }
    }

    plot(details, step, data.toArray.filter(_.photon > 0))

  }
  def plot(
      details: String,
      step: String,
      data: Array[DataLine]
  ): Unit = {

    println("plotting")
    val p = Plot.scatterPlotWithLines(data.map(_.index), data.map(_.trans))

    SwingRenderer(p, 1000, 1000, true)

  }
}
