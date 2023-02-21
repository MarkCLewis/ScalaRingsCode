package chariklosims

import data.CartAndRad
import java.io.File
import util.Particle
import scala.collection.mutable
import swiftvis2.plotting.Plot
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer

object MoonletTracker {
  def main(args: Array[String]): Unit = {
    val dir = new File(args(0))
    val dirFiles = CartAndRad.findAllInDir(dir).sortBy((_._2))
    var lastCores = Array[Int]()
    val stepFile = dirFiles.find(_._2 == 290000)
    val stepData = CartAndRad.read(new File(dir, stepFile.get._1))
    val x = stepData.map(_.x)
    val y = stepData.map(_.y)
    val diams = stepData.map(_.rad * 2)
    val colors = Array(GreenARGB, BlueARGB, RedARGB, CyanARGB, MagentaARGB, YellowARGB)
    println(s"Find moonlets, count = ${stepData.length}")
    val moonlets = locateMoonlets(stepData, lastCores)
    println("Make map")
    val moonletMap = mutable.Map[Int, Int]().withDefaultValue(BlackARGB)
    for (mi <- moonlets.indices; pi <- moonlets(mi)) moonletMap(pi) = colors(mi % colors.length)
    println("Build plot")
    val plot = Plot.scatterPlot(x, y, stepFile.get._1, "Radial", "Azimuthal", diams, symbolColor = moonletMap, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
      .updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.3f"))
    SwingRenderer(plot, 1200, 1200, true)
  }

  def locateMoonlets(data: IndexedSeq[Particle], lastCores: Array[Int]): mutable.Buffer[mutable.Buffer[Int]] = {
    // Build grid
    val grid = new ParticleGrid(data)
    val x = data.map(_.x)
    val y = data.map(_.y)
    val diams = data.map(_.rad * 2)
    val colors = Array(GreenARGB, RedARGB, CyanARGB, MagentaARGB, YellowARGB)
    val gridMap = mutable.Map[Int, Int]().withDefaultValue(BlackARGB)
    for (gi <- grid.sortedCells.indices.takeWhile(grid.sortedCells(_)._3 > 50); (cx, cy, cnt) = grid.sortedCells(gi); pi <- grid.cellIndices(cx, cy)) gridMap(pi) = colors(gi % colors.length)
    val plot1 = Plot.scatterPlot(x, y, "Grid", "Radial", "Azimuthal", diams, symbolColor = gridMap, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
      .updatedAxis[NumericAxis]("x", a => a.updatedNumberFormat("%1.3f"))
    SwingRenderer(plot1, 1200, 1200, false)

    val group = Array.fill(data.length)(-1)
    val ret = mutable.Buffer[mutable.Buffer[Int]]()
    // Run through previous cores
    // TODO: Not implemented yet.

    // Run through remaining particles by grid cell, skip low density
    for {
      (cx, cy, cnt) <- grid.sortedCells
      if cnt > 50
      indices = grid.cellIndices(cx, cy)
      if indices.nonEmpty
      pi <- indices
      if group(pi) < 0
    } {
      println(s"Cell $cx, $cy, $cnt")
      var offset = 0
      var done = false
      val included = mutable.Buffer(pi) // This could be a better data structure
      group(pi) = pi
      while (!done) {
        done = true
        for {
          nx <- cx - offset to cx + offset
          ny <- cy - offset to cy + offset
          ni <- grid.cellIndices(nx, ny).sortBy(i => data(pi).distSqr(data(i)))
          if group(ni) < 0
        } {
          // Need a spatial data structure that grows well here so I can quickly find the nearest particle in the set to the new one being tested.
          if (included.exists(inc => data(ni).overlapped(data(inc)))) { // TODO: Also needs velocity check in here.
            group(ni) = pi
            included += ni
            done = false
          }
        }
        offset += 1
      }
      if (included.length > 10) ret += included
    }
    ret
  }

  def coreParticles(data: IndexedSeq[Particle], moonlets: mutable.Buffer[mutable.Buffer[Int]]): Array[Int] = ???

}
