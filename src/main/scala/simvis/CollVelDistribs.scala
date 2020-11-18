package simvis

import data.HighVelocityCollisions
import java.net.URL
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer

object CollVelDistribs extends App {
  val impactURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/HighRes/Moonlet4d/HighVelColls.bin")
  val impacts = HighVelocityCollisions.readStream(impactURL.openStream())

  val vels = impacts.flatMap(scd => scd.colls.map(cd => cd.vel))
  val vmin = vels.min
  val vmax = vels.max
  val bins = (0 until 101).map(i => vmin + i * (vmax - vmin) / 100)
  val velHistPlot = Plot.histogramPlotFromData(bins, vels, RedARGB)
    .updatedAxis[NumericAxis]("x", _.numberFormat("%e"))
  SwingRenderer(velHistPlot, 1000, 1000, true)

  {
    val offsets = impacts.flatMap(scd => scd.colls.map { cd =>
      val dx = cd.p1.x - cd.p2.x
      val dy = cd.p1.y - cd.p2.y
      val dz = cd.p1.z - cd.p2.z
      val mag = math.sqrt(dx*dx + dy*dy + dz*dz)
      (dx/mag, dy/mag)
    })
    val xmin = offsets.map(_._1).min
    val xmax = offsets.map(_._1).max
    val ymin = offsets.map(_._2).min
    val ymax = offsets.map(_._2).max
    val binCnt = 100
    val cnts = Array.fill(binCnt, binCnt)(0.0)
    for ((dx, dy) <- offsets) {
      val xbin = ((dx - xmin) * binCnt / (xmax - xmin)).toInt min binCnt-1
      val ybin = ((dy - ymin) * binCnt / (ymax - ymin)).toInt min binCnt-1
      cnts(xbin)(ybin) += 1
    }
    val maxCnt = cnts.flatMap(row => row).max
    println(maxCnt)
    println(cnts(0).mkString(" "))
    val grad = ColorGradient(0.0 -> BlackARGB, maxCnt -> RedARGB)
    val grid = for (i <- cnts.indices; j <- cnts(i).indices) yield {
      (xmin + i * (xmax - xmin) / binCnt, ymin + j * (ymax - ymin) / binCnt, cnts(i)(j), i)
    }
    val xcolorSurface = styles.ColoredSurfaceStyle(grid.map(_._1), grid.map(_._2), grid.map(_._4): PlotIntSeries, grad(grid.map(_._3)))
    val xcolorPlot = Plot.simple(xcolorSurface)
    SwingRenderer(xcolorPlot, 1000, 1000, true)
  }

  {
    val offsets = impacts.flatMap(scd => scd.colls.map { cd =>
      val dx = cd.p1.vx - cd.p2.vx
      val dy = cd.p1.vy - cd.p2.vy
      val dz = cd.p1.vz - cd.p2.vz
      val mag = math.sqrt(dx*dx + dy*dy + dz*dz)
      (dx/mag, dy/mag)
    })
    val xmin = offsets.map(_._1).min
    val xmax = offsets.map(_._1).max
    val ymin = offsets.map(_._2).min
    val ymax = offsets.map(_._2).max
    val binCnt = 100
    val cnts = Array.fill(binCnt, binCnt)(0.0)
    for ((dx, dy) <- offsets) {
      val xbin = ((dx - xmin) * binCnt / (xmax - xmin)).toInt min binCnt-1
      val ybin = ((dy - ymin) * binCnt / (ymax - ymin)).toInt min binCnt-1
      cnts(xbin)(ybin) += 1
    }
    val maxCnt = cnts.flatMap(row => row).max
    println(maxCnt)
    println(cnts(0).mkString(" "))
    val grad = ColorGradient(0.0 -> BlackARGB, maxCnt -> RedARGB)
    val grid = for (i <- cnts.indices; j <- cnts(i).indices) yield {
      (xmin + i * (xmax - xmin) / binCnt, ymin + j * (ymax - ymin) / binCnt, cnts(i)(j), i)
    }
    val xcolorSurface = styles.ColoredSurfaceStyle(grid.map(_._1), grid.map(_._2), grid.map(_._4): PlotIntSeries, grad(grid.map(_._3)))
    val xcolorPlot = Plot.simple(xcolorSurface)
    SwingRenderer(xcolorPlot, 1000, 1000, true)
  }
}