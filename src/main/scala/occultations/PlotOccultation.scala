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
    """MeasurementDetails\((.*)\)|CartAndRad\.(\d+)\.bin|BeamSize:(.+)|Index.*|(\d+)\s+(\d+)\s+(\d+)\s+(.+)\s+(.+)\s+(.+)\s+(.+)\s+(.+)""".r

  case class DataLine(index: Int, photon: Int, trans: Int, fraction: Double, startX: Double, startY: Double, endX: Double, endY: Double)

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile(args(0))
    val lines = source.getLines()
    var details = ""
    var step = ""
    var beamSize = 0.0
    val data = collection.mutable.Buffer[DataLine]()
    for (lineRegex(md, s, bs, index, photon, trans, 
      fraction, startX, startY, endX, endY) <- lines) {

      if (md != null) {
        details = md
        if (data.nonEmpty) {
          transmissionPlot(details, step, data.toArray)
        }
      } else if (s != null) {
        step = s
      } else if (bs != null) {
        beamSize = bs.toDouble
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

    // transmissionPlot(details, step, data.toArray.filter(_.photon > 0))
    plotSample(details, step, beamSize, data.toArray.filter(_.photon > 0), "../plot/CartAndRad.40180.bin")
  }
  def transmissionPlot(details: String, step: String, data: Array[DataLine]): Unit = {
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

  def plotSample(details: String, step: String, beamSize: Double, occultationData: Array[DataLine], sampleFile: String): Unit = {
    val lineRegex = """MeasurementDetails\((.*)\)"""".r
    val data = CartAndRad.read(new File(sampleFile)).sortBy(_.z)
    val diams = data.map(_.rad * 2)

    //half transparent 7f alpha colors
    val blue = 0x6f0000ff
    val cyan = 0x6f00ffff
    val green = 0x6f00ff00
    val yellow = 0x6fffff00
    val red = 0x6fff0000
    val gradient = ColorGradient(0.0 -> blue, 0.25 -> cyan, 0.5 -> green, 0.75 -> yellow, 1.0 -> red)
    val scatter = ScatterStyle(data.map(_.x), data.map(_.y), symbolWidth = diams, symbolHeight = diams, colors = BlackARGB, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
    val oxy = occultationData.zipWithIndex.flatMap{ case (od, i) => Seq((od.startX, od.startY, od.fraction, i), (od.endX, od.endY, od.fraction, i))}
    val overPlotScatter = ScatterStyle(oxy.map(_._1), oxy.map(_._2), symbol = NoSymbol, lines = Some(LineData(oxy.map(_._4.toDouble), stroke = Renderer.StrokeData((1000*beamSize)/7))), colors = gradient(oxy.map(_._3)))
    
    //if you want to see the segments of each occultation hit more clearly pass this instead of overPlotScatter
    val segmentScatter = ScatterStyle(oxy.map(_._1), oxy.map(_._2), symbol = NoSymbol, lines = Some(LineData(oxy.map(_._4.toDouble), stroke = Renderer.StrokeData((1000*beamSize)/7))), colors = oxy.map(od => if (od._4 % 2 == 0) BlackARGB else WhiteARGB))
    
    val stackedPlot = Plot.stacked(Seq(scatter, overPlotScatter), "", "Radial", "Azimuthal")
      .updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.3g"))
      .updatedAxis[NumericAxis]("y", a => a.updatedNumberFormat("%1.6g"))

    SwingRenderer(stackedPlot, 1000, 1000, true)
    //TODO: beam Size thickness whether 1000* beamsize/7 will do and why is the gradient color different for segment and overplot
    //TODO: why the end trail gets stretched and whether that potentially shifted the regions
  }
}
