package simprocessing

import data.FixedBinned
import java.io.PrintWriter
import java.io.File

object FullWidthHalfMax extends App {
  for (arg <- args) {
    println("Processing "+arg)
    val file = new File(arg)
    val dir = file.getParentFile()
    println("Output to "+dir)
    val (names, data) = FixedBinned.read(arg)
    names.foreach(println)
    val pw = new PrintWriter(new File(dir, "fwhm.txt"))
    for (c <- data) {
      //val (inner, outer) = c.splitAt(c.length / 2)
      pw.println(s"${c(0)(0)} ${fwhm(c, 4)} ${fwhm(c, 9)} ${stdev(c, 1, 4)} ${stdev(c, 1, 9)}")
    }
    pw.close
  }

  def fwhm(col: Seq[Array[Double]], entry: Int): Double = {
    val s = 7
    val maxIndex = col.indices.maxBy(i => col(i)(entry))
    val maxData = col(maxIndex)
    val max = maxData(entry)
//    println(maxIndex, maxData.mkString("[",",","]"), max, col.length)
    val lowerIndex = (maxIndex to 0 by -1).sliding(s).find(is => is.map(i => col(i)(entry)).sum / s < max / 2).map(fnd => fnd(fnd.length/2)).getOrElse(0)
    val upperIndex = (maxIndex until col.length).sliding(s).find(is => is.map(i => col(i)(entry)).sum / s < max / 2).map(fnd => fnd(fnd.length/2)).getOrElse(col.length - 1)
    //    val center = col.sliding(s).dropWhile(_.map(_(entry)).sum / s < max / 2).takeWhile(_.map(_(entry)).sum / s > max / 2).map(_(s / 2)).toArray
    //    if(center.isEmpty) 0.0 else center.last(1) - center.head(1)
    col(upperIndex)(1) - col(lowerIndex)(1)
  }

  def stdev(col: Seq[Array[Double]], value: Int, weight: Int): Double = {
    val weightedSum = col.foldLeft(0.0)((sum, e) => sum + e(value) * e(weight))
    val weightsSum = col.foldLeft(0.0)((sum, e) => sum + e(weight))
    val wavg = weightedSum / weightsSum
    math.sqrt(col.foldLeft(0.0)((sum, e) => {
      val v = e(value) - wavg
      sum + v * v * e(weight)
    }) / weightsSum)
  }

}