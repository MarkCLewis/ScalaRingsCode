package chariklosims

import data.CartAndRad
import java.io.File
import util.Particle
import scala.collection.mutable
import swiftvis2.plotting.Plot
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.styles.ScatterStyle
import java.io.PrintWriter

object MoonletTracker {
  val MinParticlesInCoreGridCell = 100
  val MinParticleInMoonlet = 2000
  val AcceptableVelocityFactorBuild = 0.9
  val AcceptableVelocityFactorThin = 0.8
  val MoonletColors = IndexedSeq(GreenARGB, RedARGB, BlueARGB, CyanARGB, MagentaARGB, YellowARGB)

  def main(args: Array[String]): Unit = {
    val dir = new File(args(0))
    val dirFiles = CartAndRad.findAllInDir(dir).sortBy((_._2))
    val pw = new PrintWriter("coreData.txt")
    var lastCores = mutable.Map[Int, Int]()
    for ((stepFile, step) <- dirFiles) {
      println(s"Step $step")
      val stepData = CartAndRad.read(new File(dir, stepFile))
      println(s"Find moonlets, count = ${stepData.length}")
      val (moonlets, cores) = locateMoonletsAndCores(stepData, lastCores)
      println(cores)
      for (key <- cores.keys) {
        pw.println(s"$step, $key, ${cores(key)}, ${moonlets(key).length}")
        pw.flush()
      }
      if (moonlets.nonEmpty) {
        val plot = completeStepPlot(stepData, moonlets, cores, stepFile)
        SwingRenderer.saveToImage(plot, s"moonlets.$step.png", "PNG", 1800, 1200)
      }
      lastCores = cores
    }
    pw.close()
  }

  def moonletScatter(data: IndexedSeq[Particle], moonlet: mutable.Buffer[Int], core: Int, color: Int, view: Double = 0.0): ScatterStyle = {
    val parts = if (view == 0.0) {
      moonlet.map(data).sortBy(_.z)
    } else {
      val moonletSet = moonlet.toSet
      val c = data(core)
      val visibleParts = data.zipWithIndex.filter { case (p, i) => (p.x - c.x).abs < view && (p.y - c.y).abs < view }
      val (in, out) = visibleParts.partition(pi => moonletSet(pi._2))
      in.map(_._1) ++ out.map(_._1)
    }
    val x = parts.map(_.x)
    val y = parts.map(_.y)
    val diams = parts.map(_.rad * 2)
    val colors = parts.indices.map { i =>
      if (i < moonlet.length) color else 0x10000000
    }
    ScatterStyle(x, y, symbolWidth = diams, symbolHeight = diams, colors = colors, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
  }

  def filteredColorMoonletScatter(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]]): ScatterStyle = {
    val xminMoonlets = moonlets.map(_._2.minBy(i => data(i).x)).map(i => data(i).x).min
    val xmaxMoonlets = moonlets.map(_._2.maxBy(i => data(i).x)).map(i => data(i).x).max
    val ymin = data.minBy(_.y).y
    val ymax = data.maxBy(_.y).y
    val (xmin, xmax) = if (xmaxMoonlets - xminMoonlets < (ymax - ymin) / 4) {
      val cx = (xmaxMoonlets + xminMoonlets) * 0.5
      val hwidth = (ymax - ymin) / 10
      (cx - hwidth, cx + hwidth)
    } else (xmaxMoonlets, xminMoonlets)
    val filteredData = data.zipWithIndex.filter { case (p, i) => p.x >= xmin && p.x <= xmax}
    val x = filteredData.map(_._1.x)
    val y = filteredData.map(_._1.y)
    val diams = filteredData.map(_._1.rad * 2)
    val filteredDataMap = filteredData.zipWithIndex.map{ case ((p, oi), ni) => (oi, ni) }.toMap
    val moonletMap = mutable.Map[Int, Int]().withDefaultValue(BlackARGB)
    for (mi <- moonlets.keys; pi <- moonlets(mi); if filteredDataMap.contains(pi)) moonletMap(filteredDataMap(pi)) = MoonletColors(mi % MoonletColors.length)
    ScatterStyle(x, y, symbolWidth = diams, symbolHeight = diams, colors = moonletMap, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
  }

  def plotMoonlets(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, Int], title: String): Unit = {
    val x = data.map(_.x)
    val y = data.map(_.y)
    val diams = data.map(_.rad * 2)
    val colorGrad = ColorGradient(0.0 -> BlackARGB, 0.5 -> GreenARGB, 1.25 -> YellowARGB, 1.5 -> RedARGB)
    val moonletMap = mutable.Map[Int, Int]().withDefaultValue(BlackARGB)
    for (mi <- moonlets.keys; pi <- moonlets(mi)) moonletMap(pi) = colorGrad(shearRatio(data(pi), data(cores(mi))))
    val plot = Plot.scatterPlot(x, y, title, "Radial", "Azimuthal", diams, symbolColor = moonletMap, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
      .updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.3f"))
    SwingRenderer(plot, 1200, 1200, true)
  }

  def plotMoonlet(data: IndexedSeq[Particle], moonlet: mutable.Buffer[Int], core: Int, title: String): Unit = {
    val scatter = moonletScatter(data, moonlet, core, GreenARGB)
    val plot = Plot.simple(scatter, title, "Radial", "Azimuthal")
      .updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.5f"))
    SwingRenderer(plot, 1200, 1200, true)
  }

  def plotMoonletPair(data: IndexedSeq[Particle], moonlet1: mutable.Buffer[Int], core1: Int, moonlet2: mutable.Buffer[Int], core2: Int, title: String): Unit = {
    val scatter1 = moonletScatter(data, moonlet1, core1, GreenARGB, 2e-4)
    val scatter2 = moonletScatter(data, moonlet2, core2, GreenARGB, 2e-4)
    val plot = Plot.row(Array(scatter1, scatter2), title, "Radial", "Azimuthal")
      .updatedAxis[NumericAxis]("nx", a => a.updatedNumberFormat("%1.5f"))
    SwingRenderer(plot, 800, 400, true)
  }

  def plotGridOfMoonlets(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, Int], title: String): Unit = {
    val headerHeight = 0.05
    val numCols = 9
    val numRows = 8
    val width = 1.0 / numCols
    val height = (1.0 - headerHeight) / numRows
    val keys = moonlets.keys.toArray
    val grids = (for (i <- 0 until numCols * numRows; if moonlets.contains(i)) yield {
      val moonlet = moonlets(i)
      val core = cores(i)
      val plotGridX = (i % numCols).toDouble
      val plotGridY = (i / numCols).toDouble
      val style = moonletScatter(data, moonlet, core, GreenARGB, 2e-4)
      val grid = PlotGrid.oneByOne("x", Axis.ScaleStyle.Linear, "y", Axis.ScaleStyle.Linear, style)
      i.toString -> Plot.GridData(grid, Bounds(plotGridX * width, headerHeight + plotGridY * height, width, height))
    }).toMap
    val plot = Plot(Map("title" -> Plot.TextData(PlotText(title), Bounds(0, 0, 1.0, headerHeight))), grids)
    SwingRenderer(plot, 1600, 1200, true)
  }

  def completeStepPlot(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, Int], title: String): Plot = {
    val headerHeight = 0.05
    val fullPlotWidth = 0.2
    val numCols = 8
    val numRows = 7
    val width = (1.0 - fullPlotWidth) / numCols
    val height = (1.0 - headerHeight) / numRows
    val keys = moonlets.keys.toArray.sorted
    val gridsAndText = (for ((key, i) <- keys.zipWithIndex) yield {
      val moonlet = moonlets(key)
      val core = cores(key)
      val plotGridX = (i % numCols).toDouble
      val plotGridY = (i / numCols).toDouble
      val style = moonletScatter(data, moonlet, core, MoonletColors(key % MoonletColors.length), 2e-4)
      val grid = PlotGrid.oneByOne("x", Axis.ScaleStyle.Linear, "y", Axis.ScaleStyle.Linear, style)
      val gridData = Plot.GridData(grid, Bounds(plotGridX * width, headerHeight + plotGridY * height, width, height))
      val textData = Plot.TextData(PlotText(key.toString), Bounds(plotGridX * width, headerHeight + plotGridY * height, width*0.2, height*0.1))
      i.toString -> (gridData, textData)
    }).toMap
    val grids = gridsAndText.map { case (key, (grid, _)) => key -> grid }
    val texts = gridsAndText.map { case (key, (_, text)) => key -> text }
    val fullGrid = PlotGrid.oneByOne("x", Axis.ScaleStyle.Linear, "y", Axis.ScaleStyle.Linear, filteredColorMoonletScatter(data, moonlets))
    val fullGridData = Plot.GridData(fullGrid, Bounds(1.0 - fullPlotWidth, headerHeight, fullPlotWidth-0.02, 1.0 - headerHeight))
    Plot(texts + ("title" -> Plot.TextData(PlotText(title), Bounds(0, 0, 1.0, headerHeight))), grids + ("full" -> fullGridData))
  }

  def locateMoonletsAndCores(data: IndexedSeq[Particle], lastCores: mutable.Map[Int, Int]): (mutable.Map[Int, mutable.Buffer[Int]], mutable.Map[Int, Int]) = {
    // Build grid
    val grid = new ParticleGrid(data)

    val moonlets = findInitialCores(data, grid, lastCores)
    println(s"There are ${moonlets.size} moonlets.")

    val cores = coreParticles(data, moonlets)

    val thinnedMoonlets = thinMoonlets(data, moonlets, cores)
    val thinnedCores = coreParticles(data,thinnedMoonlets)
    // for (i <- (moonlets.keySet intersect thinnedMoonlets.keySet)) plotMoonletPair(data, moonlets(i), cores(i), thinnedMoonlets(i), thinnedCores(i), s"Moonlet $i, ${moonlets(i).length}, ${thinnedMoonlets(i).length}")
    
    (thinnedMoonlets, thinnedCores)
  }

  def radialVelocity(core: Particle, p: Particle): Double = {
    val dx = core.x - p.x
    val dy = core.y - p.y
    val dz = core.z - p.z
    val dmag = math.sqrt(dx * dx + dy * dy + dz * dz)
    val dvx = core.vx - p.vx
    val dvy = core.vy - p.vy
    val dvz = core.vz - p.vz
    (dx * dvx + dy * dvy + dz * dvz) / (dmag * (core.rad max p.rad))
  }

  def shearRatio(core: Particle, p: Particle): Double = {
    val dvy = (core.vy - p.vy).abs
    val dx = (core.x - p.x).abs
    dvy / (1.5 * dx)
  }

  def acceptToGroup(core: Particle, p: Particle, threshold: Double): Boolean = {
    // val dvy = (core.vy - p.vy).abs
    // val dx = (core.x - p.x).abs
    // dvy < threshold * 1.5 * (dx max (core.rad + p.rad))
    radialVelocity(core, p) < threshold
  }

  def workOutGrid(data: IndexedSeq[Particle], pi: Int, cx: Int, cy: Int, group: Array[Int], grid: ParticleGrid, useThreshold: Boolean): mutable.Buffer[Int] = {
    var offset = 0
    var done = false
    val included = mutable.Buffer(pi) // This could be a better data structure
    group(pi) = pi
    while (!done && offset < 40) {
      done = true
      for {
        nx <- cx - offset to cx + offset
        ny <- cy - offset to cy + offset
        ni <- grid.cellIndices(nx, ny).sortBy(i => data(pi).distSqr(data(i)))
        if group(ni) < 0
      } {
        // Need a spatial data structure that grows well here so I can quickly find the nearest particle in the set to the new one being tested.
        if (included.exists(inc => data(ni).overlapped(data(inc))) && (!useThreshold || acceptToGroup(data(pi), data(ni), AcceptableVelocityFactorBuild))) {
          group(ni) = pi
          included += ni
          done = false
        }
      }
      offset += 1
    }
    println(s"Offset = $offset at the end.")
    included
  }

  def findInitialCores(data: IndexedSeq[Particle], grid: ParticleGrid, lastCores: mutable.Map[Int, Int]): mutable.Map[Int, mutable.Buffer[Int]] = {
    val group = Array.fill(data.length)(-1)
    val moonlets = mutable.Map[Int, mutable.Buffer[Int]]()
    // Run through previous cores
    for ((mi, pi) <- lastCores.toSeq.sortBy(_._1)) {
      val cx = grid.xCell(data(pi).x)
      val cy = grid.yCell(data(pi).y)

      println(s"Moonlet core $cx, $cy, $mi, $pi")
      val included = workOutGrid(data, pi, cx, cy, group, grid, true)
      if (included.length > MinParticleInMoonlet) moonlets(mi) = included
    }

    // Run through remaining particles by grid cell, skip low density
    var moonletNumber = if (lastCores.isEmpty) 0 else lastCores.keys.max + 1
    for {
      (cx, cy, cnt) <- grid.sortedCells
      if cnt > MinParticlesInCoreGridCell
      indices = grid.cellIndices(cx, cy)
      cmParticle = findCMParticle(data, grid.cellIndices(cx, cy))
      pi <- indices.sortBy(i => data(cmParticle).distSqr(data(i)))
      if group(pi) < 0
    } {
      println(s"Cell $cx, $cy, $cnt, $pi, $moonletNumber")
      val included = workOutGrid(data, pi, cx, cy, group, grid, false)
      if (included.length > MinParticleInMoonlet) {
        moonlets(moonletNumber) = included
        moonletNumber += 1
      }
    }
    moonlets
  }

  def findCMParticle(data: IndexedSeq[Particle], group: mutable.Buffer[Int]): Int = {
    var cmx, cmy, cmz, mass = 0.0
    for (index <- group) {
      val r = data(index).rad
      val m = r*r*r
      mass += m
      cmx += data(index).x * m
      cmy += data(index).y * m
      cmz += data(index).z * m
    }
    cmx /= mass
    cmy /= mass
    cmz /= mass
    val ret = group.minBy(i => data(i).distSqr(cmx, cmy, cmz))
    assert(group.contains(ret))
    ret
  }

  def coreParticles(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]]): mutable.Map[Int, Int] = {
    for ((i, moonlet) <- moonlets) yield i -> findCMParticle(data, moonlet)
  }

  def thinMoonlets(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, Int]):mutable.Map[Int, mutable.Buffer[Int]] = {
    (for ((mi, moonlet) <- moonlets) yield {
      mi -> moonlet.filter { i =>
          (i == cores(mi)) || acceptToGroup(data(cores(mi)), data(i), AcceptableVelocityFactorThin)
      }
    }).filter(_._2.length > MinParticleInMoonlet)
  }

}
