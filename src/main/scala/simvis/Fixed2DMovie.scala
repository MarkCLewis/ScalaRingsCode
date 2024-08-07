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
import scala.collection.immutable.ArraySeq

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
      println("\t-width #: width of window/image in pixels, defaults to 1000")
      println("\t-height #: height of window/image in pixels, defaults to 1000")
      println("\t-display: tells if the image should be displayed in a window")
      println("\t-save prefix: tells if images should be saved and gives prefix")
      println("\t-colorIndex #: the index of the color element, default 4")
      println("\t-gradient value:hex,value:hex[,value:hex,...]. default 0.0:000000,1.0:ffffff")
      println("\t-list: if you give this argument the bin names and indices will be listed")
      println("\t-boxcar #: if specified, displays only a region around the current slide of the given size in radians")
      println("\t-azMin #: the minimum azimuthal value to display in the surface plot")
      println("\t-azMax #: the maximum azimuthal value to display in the surface plot")
      println("\t-cartXMin #: minimum radial value for the cartesian plot, default to auto")
      println("\t-cartXMax #: maximum radial value for the cartesian plot, default to auto")
      println("\t-radialNumFormat format: printf style number format for radial axes, default %1.1f")
      println("\t-cartAzimuthalNumFormat format: printf style number format for azimuthal axis in CartAndRad plot, default %1.1f")
      println("\t-azimuthalNumFormat format: printf style number format for azimuthal axis in top plot, default %1.1f")
      println("\t-valueNumFormat format: printf style number format for slice value axis, default %1.1f")
    }
    // TODO: Add axis options for number display and range.
    val dir = new File(args.sliding(2).find(_(0) == "-dir").map(_(1)).getOrElse("."))
    val start = args.sliding(2).find(_(0) == "-start").map(_(1).toInt).getOrElse(0)
    val end = args.sliding(2).find(_(0) == "-end").map(_(1).toInt).getOrElse(Int.MaxValue)
    val fixed = args.sliding(2).find(_(0) == "-fixed").map(_(1).toInt)
    val cart = args.contains("-cart")
    val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
    val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
    val display = args.contains("-display")
    val save = args.sliding(2).find(_(0) == "-save").map(_(1))
    val colorIndex = args.sliding(2).find(_(0) == "-colorIndex").map(_(1).toInt).getOrElse(4)
    val gradientString = args.sliding(2).find(_(0) == "-gradient").map(_(1)).getOrElse("0.0:000000,1.0:ffffff")
    val gradient = ColorGradient(ArraySeq.unsafeWrapArray(gradientString.split(",").map { vc =>
      val Array(v, c) = vc.split(":")
      (v.toDouble, Integer.parseInt(c, 16) | 0xff000000)
    }):_*)
    val boxcar = args.sliding(2).find(_(0) == "-boxcar").map(_(1).toDouble)
    val azMin = args.sliding(2).find(_(0) == "-azMin").map(_(1).toDouble).getOrElse(Double.MinValue)
    val azMax = args.sliding(2).find(_(0) == "-azMax").map(_(1).toDouble).getOrElse(Double.MaxValue)
    val cartXMin = args.sliding(2).find(_(0) == "-cartXMin").map(_(1).toDouble)
    val cartXMax = args.sliding(2).find(_(0) == "-cartXMax").map(_(1).toDouble)
    val radialNumFormat = args.sliding(2).find(_(0) == "-radialNumFormat").map(_(1)).getOrElse("%1.1f")
    val cartAzimuthalNumFormat = args.sliding(2).find(_(0) == "-cartAzimuthalNumFormat").map(_(1)).getOrElse("%1.1f")
    val azimuthalNumFormat = args.sliding(2).find(_(0) == "-azimuthalNumFormat").map(_(1)).getOrElse("%1.1f")
    val valueNumFormat = args.sliding(2).find(_(0) == "-valueNumFormat").map(_(1)).getOrElse("%1.1f")

    if (!display && save.isEmpty) {
      println("You must either specify -display or -save for this program to do something.")
      sys.exit(0)
    }

    val (categories, allFixedBins) = FixedBinned.read(new File(dir, "FixedBinned.bin").getAbsolutePath())
    val sgn = math.signum(allFixedBins(1)(0)(0) - allFixedBins(0)(0)(0))
    val fixedBins = allFixedBins.filter(col => col(0)(0) >= azMin && col(0)(0) <= azMax)
    if(args.contains("-list")) categories.zipWithIndex.foreach(println)

    class BinsSeries(bin: Int, startCol: Int, endCol: Int) extends PlotDoubleSeries {
      def apply(i: Int): Double = fixedBins(i / fixedBins(0).length + startCol)(i % fixedBins(0).length)(bin)
  
      def minIndex: Int = 0
      def maxIndex: Int = (endCol-startCol) * fixedBins(0).length
    }
    class SliceSeries(bin: Int, col: Seq[Array[Double]]) extends PlotDoubleSeries {
      def apply(i: Int): Double = col(i)(bin)
  
      def minIndex: Int = 0
      def maxIndex: Int = col.length
    }
    class GroupSeries(startCol: Int, endCol: Int) extends  PlotIntSeries {
      def apply(i: Int): Int = i / fixedBins(0).length
  
      def minIndex: Int = 0
      def maxIndex: Int = (endCol-startCol) * fixedBins(0).length
    }
    class StepXValues(step: Int, startCol: Int, endCol: Int) extends PlotDoubleSeries {
      def apply(i: Int): Double = (i / fixedBins(0).length + startCol)*step/1000.0
  
      def minIndex: Int = 0
      def maxIndex: Int = (endCol-startCol) * fixedBins(0).length
    }
    val updater = if (display) Some(SwingRenderer(Plot(Map.empty, Map.empty, Map.empty), width, height, true)) else None
    if (!cart) {
      for((col, index) <- fixedBins.zipWithIndex) {
        val (startCol, endCol) = boxcar.map { bc => 
          val s = fixedBins.indexWhere(c => sgn * (c(0)(0) - (col(0)(0) - sgn * bc/2)) > 0)
          val t = fixedBins.indexWhere(c => sgn * (c(0)(0) - (col(0)(0) + sgn * bc/2)) > 0, s + 1)
          s -> (if (t >= 0) t else fixedBins.length-1)
        }.getOrElse(0 -> fixedBins.length)
        val xValues = fixed.map { step => new StepXValues(step, startCol, endCol)}.getOrElse(new BinsSeries(0, startCol, endCol))
        val fixedSurface = ColoredSurfaceStyle(xValues, new BinsSeries(1, startCol, endCol), new GroupSeries(startCol, endCol), gradient(new BinsSeries(colorIndex, startCol, endCol)))
        val slicePlot = ScatterStyle(new SliceSeries(colorIndex, col), new SliceSeries(1, col), NoSymbol, lines = Some(ScatterStyle.LineData(1, Renderer.StrokeData(1, Seq(1)))))
        val grid = surfaceSliceGrid(fixedSurface, slicePlot, col.head(0), categories(colorIndex), col.maxBy(_(1)).apply(1), col.minBy(_(1)).apply(1))
        val plot = Plot(Map.empty, Map("Main" -> GridData(grid, Bounds(0, 0, 1.0, 1.0))))
          .updatedAxis[NumericAxis]("azimuthal", ax => ax.copy(tickLabelInfo = ax.tickLabelInfo.map(_.copy(numberFormat = azimuthalNumFormat))))
          .updatedAxis[NumericAxis]("value", ax => ax.copy(tickLabelInfo = ax.tickLabelInfo.map(_.copy(numberFormat = valueNumFormat))))
          .updatedAxis[NumericAxis]("radial", ax => ax.copy(tickLabelInfo = ax.tickLabelInfo.map(_.copy(numberFormat = radialNumFormat))))
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
      val filesAndNums = (for (fn @ cartRadRegex(num) <- dir.list()) yield fn -> num.toInt).sortBy(_._2)
      for ((fn, n) <- filesAndNums; if n >= start && n <= end) {
        println(fn)
        val cnr = CartAndRad.read(new File(dir, fn))
        val cnrScatter = ScatterStyle(new CartAndRadSeries(cnr, _.x), new CartAndRadSeries(cnr, _.y), symbolWidth = new CartAndRadSeries(cnr, _.rad * 2), symbolHeight = new CartAndRadSeries(cnr, _.rad *2), 
          xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
        val (col, colIndex) = fixed.map(step => fixedBins(n/step) -> n/step).getOrElse {
          val avY = cnr.foldLeft(0.0)(_ + _.y) / cnr.length
          val ind = fixedBins.indices.minBy(i => (fixedBins(i)(0)(0) - avY).abs)
          fixedBins(ind) -> ind
        }
        val (startCol, endCol) = boxcar.map { bc => 
          fixed.map{step => 
            val colWidth = (bc * 1000 / (2 * math.Pi * step)).toInt
            ((colIndex - colWidth/2) max 0, (colIndex + colWidth/2) min fixedBins.length-1)
          }.getOrElse {
            val s = fixedBins.indexWhere(c => sgn * (c(0)(0) - (col(0)(0) - sgn * bc/2)) > 0)
            val t = fixedBins.indexWhere(c => sgn * (c(0)(0) - (col(0)(0) + sgn * bc/2)) > 0, s + 1)
            s -> (if (t >= 0) t else fixedBins.length-1)
          }
        }.getOrElse(0 -> fixedBins.length)
        val xValues = fixed.map { step => new StepXValues(step, startCol, endCol)}.getOrElse(new BinsSeries(0, startCol, endCol))
        val fixedSurface = ColoredSurfaceStyle(xValues, new BinsSeries(1, startCol, endCol), new GroupSeries(startCol, endCol), gradient(new BinsSeries(colorIndex, startCol, endCol)))
        val slicePlot = ScatterStyle(new SliceSeries(colorIndex, col), new SliceSeries(1, col), NoSymbol, lines = Some(ScatterStyle.LineData(1, Renderer.StrokeData(1, Seq(1)))))
        val surfaceGrid = surfaceSliceGrid(fixedSurface, slicePlot, fixed.map(step => n / 1000.0).getOrElse(col.head(0)), categories(colorIndex), col.minBy(_(1)).apply(1), col.maxBy(_(1)).apply(1))
        val cartGrid = PlotGrid(Seq(Seq(Seq(Plot2D(cnrScatter, "x", "y")))), Map("x" -> NumericAxis.defaultHorizontalAxis("Radial", "%1.2e"), "y" -> NumericAxis.defaultVerticalAxis("Azimuthal", "%1.2e")), Seq(1), Seq(1))
        val plot = Plot(Map.empty, Map("binned" -> GridData(surfaceGrid, Bounds(0, 0, 1.0, 0.33)), "cart" -> GridData(cartGrid, Bounds(0,0.33, 1.0, 0.66))))
          .updatedAxis[NumericAxis]("azimuthal", ax => ax.copy(tickLabelInfo = ax.tickLabelInfo.map(_.copy(numberFormat = azimuthalNumFormat))), "binned")
          .updatedAxis[NumericAxis]("value", ax => ax.copy(tickLabelInfo = ax.tickLabelInfo.map(_.copy(numberFormat = valueNumFormat))), "binned")
          .updatedAxis[NumericAxis]("radial", ax => ax.copy(tickLabelInfo = ax.tickLabelInfo.map(_.copy(numberFormat = radialNumFormat))), "binned")
          .updatedAxis[NumericAxis]("x", ax => ax.copy(min = cartXMin, max = cartXMax, tickLabelInfo = ax.tickLabelInfo.map(_.copy(numberFormat = radialNumFormat))), "cart")
          .updatedAxis[NumericAxis]("y", ax => ax.copy(tickLabelInfo = ax.tickLabelInfo.map(_.copy(numberFormat = radialNumFormat))), "aximuthal")
        updater.foreach(_.update(plot))
        save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix + f".$n%06d.png", width = width, height = height))
      }
    }
  }

  def surfaceSliceGrid(fixedSurface: ColoredSurfaceStyle, slicePlot: ScatterStyle, x: Double, colorName: String, radMin: Double, radMax: Double): PlotGrid = {
    val surfaceP2D = Plot2D(fixedSurface, "azimuthal", "radial")
    val sliceP2D = Plot2D(slicePlot, "value", "radial")
    val sliceMarkerP2D = Plot2D(ScatterStyle(Array(x, x), Array(radMin, radMax), NoSymbol, lines = Some(ScatterStyle.LineData(1, Renderer.StrokeData(1, Seq(1))))), "azimuthal", "radial")
    PlotGrid(Seq(Seq(Seq(surfaceP2D, sliceMarkerP2D), Seq(sliceP2D))), Map("azimuthal" -> NumericAxis.defaultHorizontalAxis("Azimuthal/Time"), "radial" -> NumericAxis.defaultVerticalAxis("Radial", "%1.1e"), 
      "value" -> NumericAxis.defaultHorizontalAxis("value", colorName)), ArraySeq(5, 1), ArraySeq(1))
  }
}
