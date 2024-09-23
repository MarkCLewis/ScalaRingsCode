package occultations

import scala.collection.mutable
import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.styles.ScatterStyle.LineData
import data.CartAndRad
import java.io.File
import occultations.ProcessOccultations.OccultationData
import occultations.ParseToSwiftVis.lines
import util.Particle

object PlotOccultation {
  val lineRegex =
    """MeasurementDetails\((.*)\)|CartAndRad\.(\d+)\.bin|BeamSize:(.+)|Index.*|(\d+)\s+(\d+)\s+(\d+)\s+(.+)\s+(.+)\s+(.+)\s+(.+)\s+(.+)""".r

  case class DataLine(index: Int, photon: Int, trans: Int, fraction: Double, startX: Double, startY: Double, endX: Double, endY: Double)

  def main(args: Array[String]): Unit = {
    val (data, beamSize) = readOccultationFile(args(0))
    createPlot(data, args(2).toDouble, beamSize, args(1), 1000, 1000)
  }

  def readOccultationFile(occultationFile: String): (mutable.Buffer[DataLine], Double) = {
    val source = io.Source.fromFile(occultationFile)
    val lines = source.getLines()
    var details = ""
    var step = ""
    var beamSize = 0.0
    val data = mutable.Buffer[DataLine]()
    for (lineRegex(md, s, bs, index, photon, trans, 
      fraction, startX, startY, endX, endY) <- lines) {

      if (md != null) {
        details = md
        if (data.nonEmpty) {
          transmissionPlot(data.toArray)
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
    (data, beamSize)
  }

  def createPlot(data: mutable.Buffer[DataLine], R0: Double, beamSize: Double, cartAndRadFile: String, width: Int, height: Int, range: Option[(Double, Double)] = None, xOffset: Double = 0.0, outputFile: Option[String] = None): Unit = {
    // transmissionPlot(details, step, data.toArray.filter(_.photon > 0))
    val cartAndRadData = CartAndRad.read(new File(cartAndRadFile))
    val plot = plotSample(R0, beamSize, xOffset, data.toArray, cartAndRadData, height, range)
    outputFile match {
      case Some(imageFile) =>
        SwingRenderer.saveToImage(plot, imageFile, "PNG", width, height)
      case None =>
        SwingRenderer(plot, width, height, true)
    }
  }

  def transmissionPlot(data: Array[DataLine]): Unit = {
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

  def plotSample(R0: Double, beamSize: Double, xOffset: Double, occultationData: Array[DataLine], cartAndRadData: IndexedSeq[Particle], plotHeight: Int, range: Option[(Double, Double)] = None): Plot = {
    val dataByY = cartAndRadData.sortBy(_.y).drop(10000).dropRight(10000)
    val plottedHeight = (dataByY.last.y - dataByY.head.y) * R0
    val data = dataByY.sortBy(_.z)
    val diams = data.map(_.rad * 2 * R0)

    //half transparent 7f alpha colors
    val blue = 0x6f0000ff
    val cyan = 0x6f00ffff
    val green = 0x6f00ff00
    val yellow = 0x6fffff00
    val red = 0x6fff0000
    val gradient = ColorGradient(0.0 -> blue, 0.25 -> cyan, 0.5 -> green, 0.75 -> yellow, 1.0 -> red)
    val scatter = ScatterStyle(data.map(d => (d.x+xOffset) * R0), data.map(_.y * R0), symbolWidth = diams, symbolHeight = diams, colors = BlackARGB, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
    val oxy = occultationData.zipWithIndex.flatMap{ case (od, i) => Seq((od.startX, od.startY, od.fraction, i), (od.endX, od.endY, od.fraction, i))}
    println(oxy.head._2)
    val overPlotScatter = ScatterStyle(oxy.map(d => (d._1+xOffset)*R0), oxy.map(_._2*R0), symbol = NoSymbol, lines = Some(LineData(oxy.map(_._4.toDouble), 
      stroke = Renderer.StrokeData((plotHeight*beamSize)/plottedHeight))),   
      colors = gradient(oxy.map(_._3)))
    
    //if you want to see the segments of each occultation hit more clearly pass this instead of overPlotScatter
    // val segmentScatter = ScatterStyle(oxy.map(_._1), oxy.map(_._2), symbol = NoSymbol, lines = Some(LineData(oxy.map(_._4.toDouble), 
    //     stroke = Renderer.StrokeData((plotHeight*0.5*beamSize)/7))), colors = oxy.map(od => if (od._4 % 2 == 0) BlackARGB else WhiteARGB))

    val countScatter = ScatterStyle(occultationData.map(od => ((od.startX + od.endX)*0.5+xOffset)*R0), occultationData.map(_.trans), symbol = NoSymbol, 
      lines = Some(LineData(0, stroke = Renderer.StrokeData(1.0))))
    val maxCount = occultationData.foldLeft(0)(_ max _.trans)
    
    val stackedPlot = Plot.stackedGridNN(Seq(Seq(Seq(countScatter)), Seq(Seq(scatter, overPlotScatter))), "", "Radial", "Azimuthal")
      .withModifiedAxis[NumericAxis]("y", "count", a => a.copy(min = Some(0.0), max = Some(1.05 * maxCount), name = a.name.map(n => n.copy(name = "Count"))))
      .updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.3g"))
      .updatedAxis[NumericAxis]("y", a => a.updatedNumberFormat("%1.6g"))
      .updatedYAxisOnPlot(0, 0, 0, "count")

    range.fold(stackedPlot){ case (min, max) =>
      stackedPlot.updatedAxis[NumericAxis]("x", a => a.copy(min = Some(min), max = Some(max)))
    }
    //TODO: beam Size thickness whether 1000* beamsize/7 will do and why is the gradient color different for segment and overplot
    //TODO: why the end trail gets stretched and whether that potentially shifted the regions
  }
}
