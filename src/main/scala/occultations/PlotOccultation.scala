package occultations

import javafx.scene.chart.PieChart.Data
import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer
import scalafx.application.ConditionalFeature
import data.CartAndRad
import java.io.File
import scala.swing.Swing
import occultations.ProcessOccultations.OccultationData
import occultations.ParseToSwiftVis.lines
import photometry.Render
import swiftvis2.plotting.styles.ScatterStyle.LineData

object PlotOccultation {
  val lineRegex =
    """MeasurementDetails\((.*)\)|CartAndRad\.(\d+)\.bin|Index.*|(\d+)\s+(\d+)\s+(\d+)\s+(.+)\s+(.+)\s+(.+)\s+(.+)\s+(.+)""".r

  case class DataLine(index: Int, photon: Int, trans: Int, fraction: Double, startX: Double, startY: Double, endX: Double, endY: Double)

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile(args(0))
    val lines = source.getLines()
    var details = ""
    var step = ""
    val data = collection.mutable.Buffer[DataLine]()
    for (lineRegex(md, s, index, photon, trans, 
      fraction, startX, startY, endX, endY) <- lines) {

      if (md != null) {
        details = md
        if (data.nonEmpty) {
          transmitionPlot(details, step, data.toArray)
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

    // transmitionPlot(details, step, data.toArray.filter(_.photon > 0))
    plotSample(details, step, data.toArray.filter(_.photon > 0), "../plot/CartAndRad.40180.bin")
  }
  def transmitionPlot(
      details: String,
      step: String,
      data: Array[DataLine]
  ): Unit = {

    println("plotting")
    val p = Plot
      .scatterPlotWithLines(
        data.map(_.index),
        data.map(_.trans),
        "Occultation"
      )
      .updatedAxis[NumericAxis]("y", _.updatedMin(0.0))

    SwingRenderer(p, 1000, 1000, true)

  }

  def plotSample(
      details: String,
      step: String,
      occultationData: Array[DataLine],
      sampleFile: String
      ): Unit = {
    val data = CartAndRad.read(new File(sampleFile)).sortBy(_.z)
    val diams = data.map(_.rad * 2)

    val gradient = ColorGradient(0.0 -> BlueARGB, 0.25 -> CyanARGB, 0.5 -> GreenARGB, 0.75 -> YellowARGB, 1.0 -> RedARGB)

    
    val scatter = ScatterStyle(data.map(_.x), data.map(_.y), symbolWidth = diams, symbolHeight = diams, colors = BlackARGB, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
    
    val oxy = occultationData.zipWithIndex.flatMap{ case (od, i) => Seq((od.startX, od.startY, od.fraction, i), (od.endX, od.endY, od.fraction, i))}
    
    oxy.foreach(println)
    val overPlotScatter = ScatterStyle(oxy.map(_._1), oxy.map(_._2), symbol = NoSymbol, lines = Some(LineData(oxy.map(_._4.toDouble))), colors = gradient(oxy.map(_._3)))
    
    //TODO: instead of circles do lines
    
    val stackedPlot = Plot.stacked(Seq(scatter, overPlotScatter), "", "Radial", "Azimuthal")
      .updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.3g"))
      .updatedAxis[NumericAxis]("y", a => a.updatedNumberFormat("%1.4g"))
    SwingRenderer(stackedPlot, 1000, 1000, true)
    //TODO: alpha value so line is transparent and beam size thickness
  }
}
