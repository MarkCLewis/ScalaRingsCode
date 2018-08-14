package simprocessing

import data.CartAndRad
import java.io.File
import occultations.SynthOccultations
import java.io.PrintWriter

object SinglePassOccultations extends App {
  if (args.length < 2) {
    println("Requires dir and R0")
    sys.exit(0)
  }
  val dir = new File(args(0))
  val pw = new PrintWriter(new File(dir, "occultations.txt"))
  val files = dir.listFiles.filter(_.getName.startsWith("CartAndRad")).map(fn => (fn, fn.getName.split("\\.")(1).toInt)).sortBy(_._2)
  val R0 = args(1).toDouble
  for ((file, _) <- files) {
    println("reading "+file)
    val cr = CartAndRad.read(file)
    val midx = cr.map(_.x).sum / cr.length
    val midy = cr.map(_.y).sum / cr.length
    val scanLength = 1.0/R0
    val scanWidth = 0.1/R0
    val binData = SynthOccultations.binParticles(cr, scanLength*2)
    val scans = SynthOccultations.syntheticOccultation(midx, midy, 0, math.Pi / 2, 0, scanLength, 0, scanWidth, 0.0, binData, 100000)
    scans.foreach(s => pw.println(s.sx+" "+s.sy+" "+s.ex+" "+s.ey+" "+s.intensity+" "+s.photons.count(_.hit)))
  }
  pw.close
}