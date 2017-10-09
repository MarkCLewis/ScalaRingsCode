package simprocessing

import java.io.File
import java.io.FilenameFilter
import data.CartAndRad
import util.Particle
import util.GCCoord

object ProcessCartAndRad extends App {
  println("Starting up")
  val files = new File(args(0)).listFiles((d: File, s: String) => s.startsWith("CartAndRad"))
  println(files.length)
  val widthpw = new java.io.PrintWriter("width.txt")
  val taupw = new java.io.PrintWriter("tau.txt")
  val minx = -1.9e-01;
  val maxx = -1.1e-01
  val bins = 500
  for(file <- files.sortBy(f => f.getName.filter(_.isDigit).toInt)) {
    println(s"Reading $file")
    val parts = CartAndRad.read(file)
    val avy = parts.view.map(_.y).sum/parts.length
    val miny = parts.minBy(_.y).y
    val maxy = parts.maxBy(_.y).y
    val (innerRing, outerRing) = parts.partition(_.x < -0.145)
    val innerGC = innerRing.map(GCCoord.apply)
    val outerGC = outerRing.map(GCCoord.apply)
    val binned = parts.groupBy(p => ((p.x-minx)/(maxx-minx)*bins).toInt)
    val innerWidth = std(innerRing.map(_.x))
    val outerWidth = std(outerRing.map(_.x))
    val innerGCWidth = std(innerGC.map(_.X))
    val outerGCWidth = std(outerGC.map(_.X))
    widthpw.println(s"$avy $innerWidth $outerWidth $innerGCWidth $outerGCWidth")
    for(i <- 0 until bins) {
      val binx = minx+(i+0.5)*(maxx-minx)/bins
      val area = (maxy-miny)*(maxx*minx)/bins
      val covered = binned.get(i).map(ps => ps.view.map(p => p.rad*p.rad*math.Pi).sum).getOrElse(0.0)
      val tau = covered/area
      taupw.println(s"$avy $binx $tau")
    }
  }
  widthpw.close
  taupw.close
  
  def std(xs: IndexedSeq[Double]): Double = {
    val avx = xs.sum/xs.length
    math.sqrt(xs.view.map(x => x*x).sum/xs.length - avx*avx) 
  }
}