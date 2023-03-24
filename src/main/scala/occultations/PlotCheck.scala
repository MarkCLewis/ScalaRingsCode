package occultations

import scala.collection.mutable
import data.CartAndRad
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.renderer.Renderer.StrokeData

/**
 * This program is intended to do a sanity check on occultations that include the x and y locations
 * for rays.
 */
object PlotCheck {
  // val bPhiPattern = """B = (\d+), phi = ([\d.]+)""".r
  val bPhiPattern = """MeasurementDetails\([^,]+,[^,]+,([\d.]+),([\d.]+),.+\)""".r
  val simPattern = """Simulation\(([^,]*),.*\)""".r
  val orbitsPattern = """Used orbits ([\d.]+) to ([\d.]+)""".r
  val dataPattern = """(\d+)\t(\d+)\t(\d+)\t([\d.E+-]+)\t([\d.E+-]+)\t([\d.E+-]+)\t([\d.E+-]+)\t([\d.E+-]+)""".r

  case class ScanInfo(index: Int, photons: Int, trans: Int, frac: Double, sx: Double, sy: Double, ex: Double, ey: Double)

  def main(args: Array[String]): Unit = {
    if(args.length < 2) {
      println("You must specify an occultation file and a parent directory for the simulation directories. -azimuthal is optional.")
      sys.exit(0)
    }
    val (az, otherArgs) = args.partition(_ == "-azimuthal")

    val lines = scala.io.Source.fromFile(otherArgs(0)).getLines()
    val baseDir = new java.io.File(otherArgs(1))
    plotSim(lines.next(), lines, baseDir, az.nonEmpty)
  }

  def plotSim(header: String, lines: Iterator[String], baseDir: java.io.File, azimuthal: Boolean): Unit = {
    val bPhiPattern(b, phi) = header
    val simPattern(dir) = lines.next()
    val orbitsPattern(startOrbit, endOrbit) = lines.next()
    val startStep = (startOrbit.toDouble*1000).toInt
    val endStep = (endOrbit.toDouble*1000).toInt
    lines.next()
    var line = lines.next()
    var dataMatch = dataPattern.findFirstMatchIn(line)
    val scanData = mutable.Buffer[ScanInfo]()
    while(lines.hasNext && dataMatch.nonEmpty) {
      val index = dataMatch.get.group(1).toInt
      val photons = dataMatch.get.group(2).toInt
      val trans = dataMatch.get.group(3).toInt
      val frac = dataMatch.get.group(4).toDouble
      val sx = dataMatch.get.group(5).toDouble
      val sy = dataMatch.get.group(6).toDouble
      val ex = dataMatch.get.group(7).toDouble
      val ey = dataMatch.get.group(8).toDouble
      scanData += (if (azimuthal) ScanInfo(index, photons, trans, frac, sy, sx, ey, ex) else ScanInfo(index, photons, trans, frac, sx, sy, ex, ey))
      line = lines.next()
      dataMatch = dataPattern.findFirstMatchIn(line)
    }
    val cg = ColorGradient(0.0 -> BlackARGB, 0.1 -> BlueARGB, 0.2 -> GreenARGB, 0.5 -> RedARGB, 0.75 -> YellowARGB, 1.0 -> WhiteARGB)
    val occPlot = ScatterStyle(
      scanData.flatMap(si => Array(si.sx, si.ex)), 
      scanData.flatMap(si => Array(si.sy, si.ey)), 
      symbolWidth = 1.0, symbolHeight = 1.0,
      colors = cg(scanData.flatMap(si => Array(si.frac, si.frac))),
      lines = Some(ScatterStyle.LineData(SeqToIntSeries(scanData.flatMap(si => Array(si.index, si.index)).toSeq), StrokeData(2))))
    val cartFile = dir + "/CartAndRad." + endStep + ".bin"
    val cartData = CartAndRad.read(new java.io.File(cartFile))
    val sizes = cartData.map(_.rad * 2)
    val cartPlot = ScatterStyle(
      cartData.map(_.x), 
      cartData.map(_.y), 
      xSizing = PlotSymbol.Sizing.Scaled,
      ySizing = PlotSymbol.Sizing.Scaled,
      symbolWidth = sizes,
      symbolHeight = sizes)
    val phiDegrees = (phi.toDouble*180/math.Pi).round.toInt
    val plot = Plot.stacked(Seq(cartPlot, occPlot), title = s"B = $b, phi = ${phiDegrees}")
      .updatedAxis[NumericAxis]("x", _.updatedNumberFormat("%1.5e"))
      .updatedAxis[NumericAxis]("y", _.updatedNumberFormat("%1.5e"))
    val img = SwingRenderer.renderToImage(plot, 1200, 1200)
    javax.imageio.ImageIO.write(img, "PNG", new java.io.File(s"${dir.drop(dir.lastIndexOf('/')+1)}:B=$b:phi=$phiDegrees.png"))
    if(lines.hasNext) plotSim(line, lines, baseDir, azimuthal)
  }
}