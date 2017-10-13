package simprocessing

import data.FixedBinned
import java.io.PrintWriter
import java.io.File

object FullWidthHalfMax extends App {
  for (arg <- args) {
    val file = new File(arg)
    val dir = file.getParentFile()
    val (names, data) = FixedBinned.read(arg)
    val pw = new PrintWriter(new File(dir,"fwhm.txt"))
    for (c <- data) {
      val (inner, outer) = c.splitAt(c.length / 2)
      pw.println(s"${c(0)(0)} ${fwhm(inner, 4)} ${fwhm(inner, 9)} ${fwhm(outer, 4)} ${fwhm(outer, 9)}")
    }
    pw.close
  }

  def fwhm(col: Seq[Array[Double]], entry: Int): Double = {
    val s = 5
    val max = col.view.map(_(entry)).max
    val center = col.sliding(s).dropWhile(_.map(_(entry)).sum / s < max / 2).takeWhile(_.map(_(entry)).sum / s > max / 2).map(_(s / 2)).toArray
    center.last(1) - center.head(1)
  }
}