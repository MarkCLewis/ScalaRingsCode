package simvis

import java.io.File
import data.FixedBinned
import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer
import data.CartAndRad
import util.Particle

/**
  * This main is intended to read in a FixedBinned
  */
object Fixed2DMovie {
  def main(args: Array[String]): Unit = {
    if (args.contains("-help") || args.length < 1) {
      println("Arguments:")
      println("\t-dir path: path to directory, defaults to current directory")
      println("\t-fixed outputInterval: tells if the cell is fixed and if so, what the simulation output interval was")
      println("\t-cart: tells we there should be a plot frame for the CartAndRad file")
      println("\t-start #: initial step number, defaults to 0, requires cart")
      println("\t-end #: final step number, defaults to MaxInt, requires cart")
      println("\t-slice: tells if there should be a plot frame for the vertical slice through the bins")
      println("\t-width #: width of window/image in pixels, defaults to 1000")
      println("\t-height #: height of window/image in pixels, defaults to 1000")
      println("\t-display: tells if the image should be displayed in a window")
      println("\t-save prefix: tells if images should be saved and gives prefix")
      println("\t-colorIndex #: the index of teh color element, default 2")
      println("\t-gradient value:hex,value:hex[,value:hex,...]. default 0.0:000000,1.0:ffffff")
    }
    val dir = new File(args.sliding(2).find(_(0) == "-dir").map(_(1)).getOrElse("."))
    val start = args.sliding(2).find(_(0) == "-start").map(_(1).toInt).getOrElse(0)
    val end = args.sliding(2).find(_(0) == "-end").map(_(1).toInt).getOrElse(Int.MaxValue)
    val fixed = args.sliding(2).find(_(0) == "-fixed").map(_(1).toInt)
    val cart = args.contains("-cart")
    val slice = args.contains("-slice")
    val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
    val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
    val display = args.contains("-display")
    val save = args.sliding(2).find(_(0) == "-save").map(_(1))
    val colorIndex = args.sliding(2).find(_(0) == "-colorIndex").map(_(1).toInt).getOrElse(2)
    val gradientString = args.sliding(2).find(_(0) == "gradient").map(_(2)).getOrElse("0.0:000000,1.0:ffffff")
    val gradient = ColorGradient(gradientString.split(",").map { vc =>
      val Array(v, c) = vc.split(":")
      (v.toDouble, Integer.parseInt(c, 16) | 0xff000000)
    }:_*)

    if (!display && save.isEmpty) {
      println("You must either specify -display or -save for this program to do something.")
      sys.exit(0)
    }

    val (categories, fixedBins) = FixedBinned.read(new File(dir, "FixedBinned.bin").getAbsolutePath())
    class BinsSeries(bin: Int) extends PlotDoubleSeries {
      def apply(i: Int): Double = fixedBins(i / fixedBins(0).length)(i % fixedBins(0).length)(bin)
  
      def minIndex: Int = 0
      def maxIndex: Int = fixedBins.length * fixedBins(0).length
    }
    class SliceSeries(bin: Int, col: Seq[Array[Double]]) extends PlotDoubleSeries {
      def apply(i: Int): Double = col(i)(bin)
  
      def minIndex: Int = 0
      def maxIndex: Int = col.length
    }
    val groupSeries = new PlotIntSeries {
      def apply(i: Int): Int = i / fixedBins(0).length
  
      def minIndex: Int = 0
      def maxIndex: Int = fixedBins.length * fixedBins(0).length
    }
    val colorSeries = new PlotIntSeries {
      def apply(i: Int): Int = fixedBins(i / fixedBins(0).length)(i % fixedBins(0).length)(2).toInt // TODO: Fix colors
  
      def minIndex: Int = 0
      def maxIndex: Int = fixedBins.length * fixedBins(0).length
    }
    val fixedSurface = ColoredSurfaceStyle(new BinsSeries(0), new BinsSeries(1),groupSeries, colorSeries)
    val updater = if (display) Some(SwingRenderer(Plot(Map.empty, Map.empty, Seq.empty), width, height, true)) else None
    if (!cart) {
      for((col, index) <- fixedBins.zipWithIndex) {
        val slicePlot = ScatterStyle(new SliceSeries(colorIndex, col), new SliceSeries(1, col), NoSymbol, lines = Some(ScatterStyle.LineData(1, Renderer.StrokeData(1, Seq(1)))))
        val grid = surfaceSliceGrid(fixedSurface, slicePlot, col.head(0), categories(colorIndex), col.maxBy(_(1)).apply(1), col.minBy(_(1)).apply(1))
        val plot = Plot(Map.empty, Map("Main" -> GridData(grid, Bounds(0, 0, width, height))))
        updater.foreach(_.update(plot))
        save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix + s".$index.png", width = width, height = height))
      }
    } else {
      class CartAndRadSeries(cnr: IndexedSeq[Particle], elem: Particle => Double) extends PlotDoubleSeries {
        def apply(i: Int): Double = elem(cnr(i))
    
        def minIndex: Int = 0
        def maxIndex: Int = cnr.length
      }
      val cartRadRegex = """CartAndRad\.(\d+)\.bin""".r
      for (fn @ cartRadRegex(num) <- dir.list(); n = num.toInt; if n >= start && n <= end) {
        val cnr = CartAndRad.read(new File(dir, fn))
        val cnrScatter = ScatterStyle(new CartAndRadSeries(cnr, _.x), new CartAndRadSeries(cnr, _.y), symbolWidth = new CartAndRadSeries(cnr, _.rad * 2), symbolHeight = new CartAndRadSeries(cnr, _.rad *2), 
          xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
        val avX = cnr.foldLeft(0.0)(_ + _.x) / cnr.length
        val col = fixedBins.minBy(c => (c(0)(0) - avX).abs)
        val slicePlot = ScatterStyle(new SliceSeries(colorIndex, col), new SliceSeries(1, col), NoSymbol, lines = Some(ScatterStyle.LineData(1, Renderer.StrokeData(1, Seq(1)))))
        val surfaceGrid = surfaceSliceGrid(fixedSurface, slicePlot, col.head(0), categories(colorIndex), col.maxBy(_(1)).apply(1), col.minBy(_(1)).apply(1))
        val cartGrid = PlotGrid(Seq(Seq(Seq(Plot2D(cnrScatter, "x", "y")))), Map("x" -> NumericAxis("x"), "y" -> NumericAxis("y")), Seq(1), Seq(1))
        val plot = Plot(Map.empty, Map("binned" -> GridData(surfaceGrid, Bounds(0, 0, width, height / 3)), "cart" -> GridData(cartGrid, Bounds(0, height / 3, width, 2 * height / 3))))
        updater.foreach(_.update(plot))
        save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix + s".$num.png", width = width, height = height))
      }
    }
  }

  def surfaceSliceGrid(fixedSurface: ColoredSurfaceStyle, slicePlot: ScatterStyle, x: Double, colorName: String, radMin: Double, radMax: Double): PlotGrid = {
    val surfaceP2D = Plot2D(fixedSurface, "azimuthal", "radial")
    val sliceP2D = Plot2D(slicePlot, "value", "radial")
    val sliceMarkerP2D = Plot2D(ScatterStyle(Array(x, x), Array(radMin, radMax), NoSymbol, lines = Some(ScatterStyle.LineData(1, Renderer.StrokeData(1, Seq(1))))), "azimuthal", "radial")
    PlotGrid(Seq(Seq(Seq(surfaceP2D, sliceMarkerP2D), Seq(sliceP2D))), Map("azimuthal" -> NumericAxis("azimuthal"), "radial" -> NumericAxis("radial"), "value" -> NumericAxis("value")), Array(6, 1), Array(1))
  }
}
