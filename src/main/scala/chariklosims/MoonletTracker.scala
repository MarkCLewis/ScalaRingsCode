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

case class IndexAndStep(index: Int, lostSteps: Int)

object MoonletTracker {
  val MinParticlesInCoreGridCell = 100
  val MinParticleInMoonlet = 2000
  val AcceptableVelocityFactorThin = 0.8
  val StepsToKeep = 10
  val CentralMass = 5e18
  val R0 = 391e3 / (2.0 / (3.0 * 4) + 1.0)
  val ParticleDensityFactor = 500 * (R0 * R0 * R0) * 1.3333333 * math.Pi / CentralMass
  println(s"density = $ParticleDensityFactor")
  val MoonletColors = IndexedSeq(GreenARGB, RedARGB, BlueARGB, CyanARGB, MagentaARGB, YellowARGB)

  def main(args: Array[String]): Unit = {
    val dir = new File(args(0))
    val tolerance = args(1).toDouble
    val dirFiles = CartAndRad.findAllInDir(dir).sortBy((_._2))
    val pw = new PrintWriter("coreData.txt")
    var lastCores = mutable.Map[Int, IndexAndStep]()
    for ((stepFile, step) <- dirFiles) {
      println(s"Step $step")
      val stepData = CartAndRad.read(new File(dir, stepFile))
      if (stepData.isEmpty) {
        println(s"Reading $stepFile gave zero particles.")
      } else {
        println(s"Find moonlets, count = ${stepData.length}")
        val (moonlets, cores) = locateMoonletsAndCores(stepData, lastCores, tolerance)
        println(cores)
        for (key <- cores.keys) {
          val moonletSize = if(moonlets.contains(key)) moonlets(key).length else 0
          pw.println(s"$step, $key, ${cores(key)}, ${moonletSize}")
          pw.flush()
        }
        if (cores.nonEmpty) {
          val plot = completeStepPlot(stepData, moonlets, cores, stepFile)
          SwingRenderer.saveToImage(plot, s"moonlets.$step.png", "PNG", 3600, 2400)
        }
        lastCores = cores
      }
    }
    pw.close()
  }

  def moonletScatter(data: IndexedSeq[Particle], moonlet: mutable.Buffer[Int], core: Int, color: Int, view: Double = 0.0, defaultPartColor: Int = 0x10000000): ScatterStyle = {
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
      if (i < moonlet.length) color else defaultPartColor
    }
    ScatterStyle(x, y, symbolWidth = diams, symbolHeight = diams, colors = colors, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
  }

  def filteredColorMoonletScatter(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, IndexAndStep], palette: IndexedSeq[Int] = MoonletColors): ScatterStyle = {
    val xminMoonlets = if(moonlets.isEmpty) {
      cores.map { case (mi, IndexAndStep(pi, s)) => data(pi).x }.min
    } else moonlets.map(_._2.minBy(i => data(i).x)).map(i => data(i).x).min
    val xmaxMoonlets = if(moonlets.isEmpty) {
      cores.map { case (mi, IndexAndStep(pi, s)) => data(pi).x }.max
    } else moonlets.map(_._2.maxBy(i => data(i).x)).map(i => data(i).x).max
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
    for (mi <- moonlets.keys; pi <- moonlets(mi); if filteredDataMap.contains(pi)) moonletMap(filteredDataMap(pi)) = palette(mi % palette.length)
    ScatterStyle(x, y, symbolWidth = diams, symbolHeight = diams, colors = moonletMap, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
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
    val plot = Plot.row(Array(scatter1, scatter2).toIndexedSeq, title, "Radial", "Azimuthal")
      .updatedAxis[NumericAxis]("nx", a => a.updatedNumberFormat("%1.5f"))
    SwingRenderer(plot, 800, 400, true)
  }

  def plotGridOfMoonlets(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, IndexAndStep], title: String): Unit = {
    val headerHeight = 0.05
    val numCols = 9
    val numRows = 8
    val width = 1.0 / numCols
    val height = (1.0 - headerHeight) / numRows
    val keys = moonlets.keys.toArray
    val grids = (for (i <- 0 until numCols * numRows; if moonlets.contains(i)) yield {
      val moonlet = moonlets(i)
      val core = cores(i).index
      val plotGridX = (i % numCols).toDouble
      val plotGridY = (i / numCols).toDouble
      val style = moonletScatter(data, moonlet, core, GreenARGB, 2e-4)
      val grid = PlotGrid.oneByOne("x", Axis.ScaleStyle.Linear, "y", Axis.ScaleStyle.Linear, style)
      i.toString -> Plot.GridData(grid, Bounds(plotGridX * width, headerHeight + plotGridY * height, width, height))
    }).toMap
    val plot = Plot(Map("title" -> Plot.TextData(PlotText(title), Bounds(0, 0, 1.0, headerHeight))), grids)
    SwingRenderer(plot, 1600, 1200, true)
  }

  def completeStepPlot(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]], cores: mutable.Map[Int, IndexAndStep], title: String): Plot = {
    val headerHeight = 0.05
    val fullPlotWidth = 0.2
    val defaultCols = 1
    val defaultRows = 1
    val (numCols, numRows) = if (cores.size <= defaultRows * defaultCols) (defaultCols, defaultRows) else {
      val sqrt = math.sqrt(cores.size).toInt
      (sqrt, sqrt + 1)
    }
    val width = (1.0 - fullPlotWidth) / numCols
    val height = (1.0 - headerHeight) / numRows
    val keys = cores.keys.toArray.sorted
    val gridsAndText = (for ((key, i) <- keys.zipWithIndex) yield {
      val moonlet = if(moonlets.contains(key)) moonlets(key) else mutable.Buffer.empty[Int]
      val core = cores(key).index
      val plotGridX = (i % numCols).toDouble
      val plotGridY = (i / numCols).toDouble
      val style = moonletScatter(data, moonlet, core, MoonletColors(key % MoonletColors.length), 3e-4)
      val grid = PlotGrid.oneByOne("x", Axis.ScaleStyle.Linear, "y", Axis.ScaleStyle.Linear, style)
        .withModifiedAxis[NumericAxis]("x", "x", a => a.updatedNumberFormat("%1.5f"))
        .withModifiedAxis[NumericAxis]("y", "y", a => a.updatedNumberFormat("%1.5f"))
      val gridData = Plot.GridData(grid, Bounds(plotGridX * width, headerHeight + plotGridY * height, width, height))
      val textData = Plot.TextData(PlotText(key.toString), Bounds(plotGridX * width, headerHeight + plotGridY * height, width*0.2, height*0.1))
      i.toString -> (gridData, textData)
    }).toMap
    val grids = gridsAndText.map { case (key, (grid, _)) => key -> grid }
    val texts = gridsAndText.map { case (key, (_, text)) => key -> text }
    val fullGrid = PlotGrid.oneByOne("x", Axis.ScaleStyle.Linear, "y", Axis.ScaleStyle.Linear, filteredColorMoonletScatter(data, moonlets, cores))
    val fullGridData = Plot.GridData(fullGrid, Bounds(1.0 - fullPlotWidth, headerHeight, fullPlotWidth-0.02, 1.0 - headerHeight))
    Plot(texts + ("title" -> Plot.TextData(PlotText(title), Bounds(0, 0, 1.0, headerHeight))), grids + ("full" -> fullGridData))
  }

  def locateMoonletsAndCores(data: IndexedSeq[Particle], lastCores: mutable.Map[Int, IndexAndStep], tolerance: Double): (mutable.Map[Int, mutable.Buffer[Int]], mutable.Map[Int, IndexAndStep]) = {
    // Build grid
    val grid = new ParticleGrid(data)

    val moonlets = findMoonlets(data, grid, lastCores, tolerance)
    println(s"There are ${moonlets.size} moonlets.")

    val cores = coreParticles(data, moonlets)

    val moonletSet = mutable.Set[Int]()
    for (mi <- moonlets.keys; pi <- moonlets(mi)) moonletSet += pi
    for ((mi, IndexAndStep(pi, cnt)) <- lastCores; if !moonletSet(pi) && cnt < StepsToKeep) {
      cores(mi) = IndexAndStep(pi, cnt + 1)
    }

    (moonlets, cores)
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

  def acceptToGroup(core: Particle, p: Particle, threshold: Double): Boolean = {
    radialVelocity(core, p) < threshold
  }

  def workOutGrid(data: IndexedSeq[Particle], pi: Int, cx: Int, cy: Int, group: Array[Int], grid: ParticleGrid, useThreshold: Boolean, tolerance: Double): mutable.Buffer[Int] = {
    var offset = 0
    var done = false
    val included = new OverlapFinder(data, pi, tolerance) // This could be a better data structure
    group(pi) = pi
    val mass = particleMass(data(pi).rad)
    var cmx = data(pi).x * mass
    var cmy = data(pi).y * mass
    var cmz = data(pi).z * mass
    var cmvx = data(pi).vx * mass
    var cmvy = data(pi).vy * mass
    var cmvz = data(pi).vz * mass
    var cmass = mass
    while (!done && offset < 40) {
      done = true
      for {
        nx <- cx - offset to cx + offset
        ny <- cy - offset to cy + offset
        ni <- grid.cellIndices(nx, ny).sortBy(i => data(i).distSqr(cmx, cmy, cmz))
        if group(ni) < 0
      } {
        // Need a spatial data structure that grows well here so I can quickly find the nearest particle in the set to the new one being tested.
        if (isGravitationallyBound(data(ni), cmx/cmass, cmy/cmass, cmz/cmass, cmvx/cmass, cmvy/cmass, cmvz/cmass, cmass)
            && included.checkParticle(ni)) {
          group(ni) = pi
          included.addParticle(ni)
          val mass = particleMass(data(pi).rad)
          cmass += mass
          cmx += data(ni).x * mass
          cmy += data(ni).y * mass
          cmz += data(ni).z * mass
          cmvx += data(ni).vx * mass
          cmvy += data(ni).vy * mass
          cmvz += data(ni).vz * mass
          done = false
        }
      }
      offset += 1
    }
    // println(s"Offset = $offset at the end.")
    included.parts
  }

  def findMoonlets(data: IndexedSeq[Particle], grid: ParticleGrid, lastCores: mutable.Map[Int, IndexAndStep], tolerance: Double): mutable.Map[Int, mutable.Buffer[Int]] = {
    val group = Array.fill(data.length)(-1)
    val moonlets = mutable.Map[Int, mutable.Buffer[Int]]()
    // Run through previous cores
    for ((mi, IndexAndStep(pi, _)) <- lastCores.toSeq.sortBy(_._1)) {
      val cx = grid.xCell(data(pi).x)
      val cy = grid.yCell(data(pi).y)

      // println(s"Moonlet core $cx, $cy, $mi, $pi")
      val included = workOutGrid(data, pi, cx, cy, group, grid, true, tolerance)
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
      // println(s"Cell $cx, $cy, $cnt, $pi, $moonletNumber")
      val included = workOutGrid(data, pi, cx, cy, group, grid, false, tolerance)
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

  def coreParticles(data: IndexedSeq[Particle], moonlets: mutable.Map[Int, mutable.Buffer[Int]]): mutable.Map[Int, IndexAndStep] = {
    for ((i, moonlet) <- moonlets) yield {
      val newCore = findCMParticle(data, moonlet)
      i -> IndexAndStep(newCore, 0)
    }
  }

  def particleMass(rad: Double): Double = ParticleDensityFactor * rad * rad * rad

  def isGravitationallyBound(p: Particle, cmx: Double, cmy: Double, cmz: Double, cmvx: Double, cmvy: Double, cmvz: Double, cmass: Double): Boolean = {
    val dx = p.x - cmx
    val dy = p.y - cmy
    val dz = p.z - cmz
    val dist = math.sqrt(dx*dx + dy*dy + dz*dz)
    val dvx = p.vx - cmvx
    val dvy = p.vy - cmvy
    val dvz = p.vz - cmvz
    val keng = 0.5*(dvx*dvx + dvy*dvy + dvz*dvz)
    val peng = cmass / dist
    // println(s"keng = $keng, peng = $peng, dist = $dist, cmass = $cmass")
    keng < peng
  }
}
