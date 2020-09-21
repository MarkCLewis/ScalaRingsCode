package simvis

import swiftvis2.plotting.styles.ColoredSurfaceStyle
import swiftvis2.plotting.ColorGradient

object Fixed2DAnnulus {
  def main(args: Array[String]): Unit = {

  }

  /**
    * This method builds a ColoredSurfaceStyle for 2PI radians of data pulled from a FixedBinned data set.
    *
    * @param fixedData The data from the file.
    * @param azimuthalIndex the index of the azimuthal value in the data.
    * @param radialIndex The index of the radial value in the data.
    * @param valueIndex The index of the value we want in the annulus.
    * @param startIndex The index in the data used for the beginning of the plot.
    * @param gradient The color gradient used to transform the values.
    * @return
    */
  def coloredAnnulus(fixedData: Seq[Seq[Array[Double]]], azimuthalIndex: Int, radialIndex: Int, valueIndex: Int, startIndex: Int, gradient: ColorGradient): ColoredSurfaceStyle = {
    val rmin = fixedData.foldLeft(Double.MaxValue)(_ min _.foldLeft(Double.MaxValue)(_ min _(valueIndex)))
    val rmax = fixedData.foldLeft(Double.MaxValue)(_ max _.foldLeft(Double.MaxValue)(_ max _(valueIndex)))
    var i = startIndex
    val xs = collection.mutable.ArrayBuffer[Double]()
    while (i < fixedData.length && fixedData(i)(0)(azimuthalIndex) < fixedData(startIndex)(0)(azimuthalIndex) + 2 * math.Pi) {

    }
    ???
  }
}