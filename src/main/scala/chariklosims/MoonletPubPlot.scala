package chariklosims

import scala.collection.immutable.ArraySeq
import java.io.File
import swiftvis2.plotting._
import scala.collection.mutable
import data.CartAndRad
import util.Particle
import swiftvis2.plotting.renderer.SwingRenderer

object MoonletPubPlot {
  val MoonletColors = IndexedSeq(0xFFE69F00, 0xFF56B4E9, 0xFF009E73, 0xFFF0E442, 0xFF0072B2, 0xFFD55E00, 0xFFCC79A7)

  def hardSpherePlot1(): (String, Int, mutable.Map[Int, IndexAndStep], Int, Int) = {
    val cartDirectory = "/home/mlewis/Rings/CentaurRings/Chariklo/InnerRing-grav-rho0.5-tau0.1-hard/"
    val stepNum = 32480
    val cores = mutable.Map(0 -> IndexAndStep(221293, 0), 1 -> IndexAndStep(1522795, 0))
    (cartDirectory, stepNum, cores, 2857, 4000)
  }
  def softSpherePlot1(): (String, Int, mutable.Map[Int, IndexAndStep], Int, Int) = {
    val cartDirectory = "/data/mlewis/Rings/CentaurRings/Chariklo/InnerRing-grav-rho0.5-tau0.1/"
    val stepNum = 270800
    val cores = Seq(222457, 396777, 541902, 1960864, 2276304, 307963, 2279675, 2158304, 315757)
    (cartDirectory, stepNum, mutable.Map(cores.indices.map { i => i -> IndexAndStep(cores(i), 0) }:_*), 5200, 4000)
  }
  def softSpherePlot2(): (String, Int, mutable.Map[Int, IndexAndStep], Int, Int) = {
    val cartDirectory = "/data/mlewis/Rings/CentaurRings/Chariklo/InnerRing-grav-rho0.5-tau0.1/"
    val stepNum = 310600
    val cores = Seq(2123769, 349732, 1067593, 2319368, 238115, 737951, 2115689, 2263152, 343257)
    (cartDirectory, stepNum, mutable.Map(cores.indices.map { i => i -> IndexAndStep(cores(i), 0) }:_*), 5200, 4000)
  }


  def main(args: Array[String]): Unit = {
    val defFuncs = Seq(
      hardSpherePlot1 _,
      softSpherePlot1 _,
      softSpherePlot2 _
    )
    for (func <- defFuncs) {
      val (cartDirectory, stepNum, lastCores, width, height) = func()

      val cartData = CartAndRad.read(new File(s"$cartDirectory/CartAndRad.$stepNum.bin"))
      val miny = cartData.minBy(_.y).y
      val maxy = cartData.maxBy(_.y).y
      val midy = 0.5 * (miny + maxy)
      val data = cartData.map { cd =>
        val x = 391-((1.0 + cd.x) * 335 - 391)
        val y = -(cd.y - midy) * 335
        val z = cd.z * 335
        val vx = cd.vx
        val vy = cd.vy
        val vz = cd.vz
        val rad = cd.rad * 335
        Particle(x, y, z, vx, vy, vz, rad)
      }
      val (allMoonlets, allCores) = MoonletTracker.locateMoonletsAndCores(data, lastCores)
      val moonlets = lastCores.map { case (core, _) => core -> allMoonlets(core) }
      val cores = lastCores.map { case (core, _) => core -> allCores(core) }
      // val plot = completeStepPlot(data, moonlets, cores)
      // SwingRenderer.saveToImage(plot, s"pub-moonlets.$stepNum.png", "PNG", width, height)

      val sepsMap = minSeparations(data, moonlets, cores)
      val bins = (0 to 100).map(i => 0.01 * i)
      for ((key, seps) <- sepsMap) {
        val fracSeps = seps.map(_._2)
        SwingRenderer(Plot.histogramPlotFromData(bins, fracSeps, BlackARGB, s"Moonet $key", "Fractional Separation", "Count"), 1200, 1200, true)
      }
    }
  }

  def minSeparations(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, IndexAndStep]): Map[Int, Seq[(Double, Double)]] = {
    (for (key <- moonlets.keys) yield {
      val core = cores(key).index
      val overlap = new OverlapFinder(data, core)
      for (i <- moonlets(key)) {
        if (i != core) overlap.addParticle(i)
      }
      val moonlet = moonlets(key)
      val minSep = moonlet.map { i =>
        val p1 = data(i)
        val p2 = data(overlap.closestNeighbor(i))
        val sep = math.sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y) + (p1.z - p2.z) * (p1.z - p2.z))
        (sep, sep / (p1.rad + p2.rad))
      }
      key -> minSep.sortBy(_._2).toSeq
    }).toMap
  }

  def completeStepPlot(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, IndexAndStep]): Plot = {
    val fullPlotWidth = 0.3
    val defaultCols = 1
    val defaultRows = 1
    val (numCols, numRows) = if (cores.size <= defaultRows * defaultCols) (defaultCols, defaultRows) else {
      val sqrt = math.sqrt(cores.size)
      (math.floor(sqrt).toInt, math.ceil(sqrt).toInt)
    }
    val width = (1.0 - fullPlotWidth) / numCols
    val height = 0.98 / numRows
    val keys = cores.keys.toArray.sorted
    val grids = (for ((key, i) <- keys.zipWithIndex) yield {
      val moonlet = if(moonlets.contains(key)) moonlets(key) else mutable.Buffer.empty[Int]
      val core = cores(key).index
      val plotGridX = (i % numCols).toDouble
      val plotGridY = (i / numCols).toDouble
      val style = MoonletTracker.moonletScatter(data, moonlet, core, MoonletColors(key % MoonletColors.length), 0.2, 0x77000000)
      val grid = PlotGrid.oneByOne("x", Axis.ScaleStyle.Linear, "y", Axis.ScaleStyle.Linear, style)
        .withModifiedAxis[NumericAxis]("x", "x", a => a.updatedNumberFormat("%1.2f").updatedName("Radial Distance from Chariklo [km]"))
        .withModifiedAxis[NumericAxis]("y", "y", a => a.updatedNumberFormat("%1.1f").updatedName("Azimuthal Offset from Cell Center [km]"))
      val gridData = Plot.GridData(grid, Bounds(plotGridX * width, plotGridY * height + 0.01, width, height))
      i.toString -> gridData
    }).toMap
    val fullGrid = PlotGrid.oneByOne("x", Axis.ScaleStyle.Linear, "y", Axis.ScaleStyle.Linear, MoonletTracker.filteredColorMoonletScatter(data, moonlets, cores, MoonletColors))
      .withModifiedAxis[NumericAxis]("x", "x", a => a.updatedNumberFormat("%1.2f").updatedName("Radial Distance from Chariklo [km]"))
      .withModifiedAxis[NumericAxis]("y", "y", a => a.updatedNumberFormat("%1.1f").updatedName("Azimuthal Offset from Cell Center [km]").updatedMax(3.0).updatedMin(-3.0))
    val fullGridData = Plot.GridData(fullGrid, Bounds(1.0 - fullPlotWidth, 0.01, fullPlotWidth-0.02, 0.98))
    Plot(Map.empty, grids + ("full" -> fullGridData))
  }
}