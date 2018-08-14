package data

import java.io._

object FixedBinned {
  def read(dataFile: String): (Array[String], Seq[Seq[Array[Double]]]) = {
    val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(dataFile)))
    val binIn = new BinaryInput(dis, true)
    val binCountSecondary = binIn.readInt4()
    val binCountPrimary = binIn.readInt4()
    val minSecondary = binIn.readReal4()
    val maxSecondary = binIn.readReal4()
    val minPrimary = binIn.readReal4()
    val maxPrimary = binIn.readReal4()
    val variablePrimary = (binCountPrimary < 2)
    val paramNames = Array.fill(binIn.readInt4())(binIn.readCString())
    val numKeepParams = paramNames.length
    val valueNames = Array.fill(binIn.readInt4())(binIn.readCString())
    val secondarySpace = (maxSecondary - minSecondary) / binCountSecondary
    val primarySpace = if (binCountPrimary > 0) (maxPrimary - minPrimary) / binCountPrimary else 0
    var primaryVal = minPrimary + 0.5 * primarySpace
    val skipCount = 0
    var i = 0
    val dataVect = collection.mutable.Buffer[Seq[Array[Double]]]()
    try {
      while (dis.available > 7 || i < binCountPrimary) {
        var secondaryVal = minSecondary + 0.5 * secondarySpace
        if (variablePrimary) primaryVal = binIn.readReal4()
        val col = for (j <- 0 until binCountSecondary) yield {
          val vals = Array.fill(numKeepParams + valueNames.length + 2)(0.0)
          vals(0) = primaryVal
          vals(1) = secondaryVal
          for (k <- 0 until paramNames.length) {
            vals(k + 2) = binIn.readInt4()
          }
          for (k <- 0 until valueNames.length) {
            vals(k + 2 + numKeepParams) = binIn.readReal4()
          }
          secondaryVal += secondarySpace
          vals
        }
        dataVect += col
        primaryVal += primarySpace
        i += 1
      }
    } catch {
      case ex: EOFException =>
    } finally {
      binIn.close()
    }
    (paramNames ++ valueNames, dataVect)
  }
}