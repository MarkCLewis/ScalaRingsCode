package simprocessing


object MinMaxFinder {
  def apply(x: Seq[Double], y: Seq[Double], window: Int): Seq[ExtremaFit] = {
    // f_0 = x^2
    // f_1 = x
    // f_2 = 1
    //     |f_0(x_0) f_1(x_0) f_2(x_0)|
    // D = |f_0(x_1) f_1(x_1) f_2(x_1)|
    //     |...      ...      ...     |
    //     |f_0(x_n) f_1(x_n) f_2(x_n)|
    // Ax=b, A = D^T*D, b = D^T*y
    // Do running quadratic fit. D^T*D and D^T*y can be calculated in a running way by adding and removing a row from D and y.

    // If fit location isn't in the window it is thrown out immediately.

    // For remaining, fits need to find those nearby one another and somehow combine the ones that agree. This cold just be a fit with a larger window.
    
    ???
  }

  // Hard code this to 3x3
  def doFit(aMat: Array[Array[Double]], b: Array[Double]): ExtremaFit = {
    ???
  }

  // This class represents a quadratic fit to a section of the data. ax^2+bx+c
  case class ExtremaFit(a: Double, b: Double, c: Double) {
    // Solves 2ax+b = 0 to find the location of the extrema
    def location: Double = -0.5 * b / a

    def isMax = a < 0.0
  }
}