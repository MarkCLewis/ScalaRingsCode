package simprocessing


object MinMaxFinder {
  //this is some test code I lazily put in here for now
  def main(args: Array[String]): Unit = {
    val x = Seq(1.0,2.0,4.0,6.0)
    val y = Seq(4.0,8.0,8.0,-1.0)
    val window = 3
    val out = apply(x,y,window)
    println("number of windows observed:", out.size)
    for(i <- 0 until out.size){
      println("extrema location:",out(i).location)
    }
  }

  def apply(x: Seq[Double], y: Seq[Double], window: Int): Seq[ExtremaFit] = {
    //this should actually be of mutable size so I can grow it according to the number of extrema I keep
    val retArr = Array.ofDim[ExtremaFit](x.size-window+1)
    val sz = 3
    // f_0 = x^2
    // f_1 = x
    // f_2 = 1
    var f0 = (z:Double) => z*z
    var f1 = (z:Double) => z
    var f2 = (z:Double) => 1.0
    val funcs = Array[(Double)=>Double](f0,f1,f2)
    //     |f_0(x_0) f_1(x_0) f_2(x_0)|
    // D = |f_0(x_1) f_1(x_1) f_2(x_1)|
    //     |...      ...      ...     |
    //     |f_0(x_n) f_1(x_n) f_2(x_n)|
    // Ax=b, A = D^T*D, b = D^T*y
    // Do running quadratic fit. D^T*D and D^T*y can be calculated in a running way by adding and removing a row from D and y.
    //I have NOT yet implemented the running method
    val D = Array.ofDim[Double](window,sz)
    val transD = Array.ofDim[Double](sz,window)
    for(slider <- 0 until x.size+1-window){
      val xSlice = x.slice(slider,slider+window)
      val ySlice = y.slice(slider,slider+window)
      for(i <- 0 until window){
        for(j <- 0 until sz){
          D(i)(j) = funcs(j)(xSlice(i))
          transD(j)(i) = D(i)(j)
        }
      }
      //printMatrix("D",D)
      //printMatrix("transD",transD)
      val aMat = matMult(transD,D)
      val b = matMult(transD,ySlice.toArray)
      //printMatrix("A",aMat)
      //b.foreach(println(_))
      retArr(slider) = doFit(aMat,b)
    }

    //HAVE NOT YET DONE THESE 2:
    // If fit location isn't in the window it is thrown out immediately.
    // For remaining, fits need to find those nearby one another and somehow combine the ones that agree. This could just be a fit with a larger window.
    
    retArr.toSeq
  }

  // Hard code this to 3x3
  def doFit(aMat: Array[Array[Double]], b: Array[Double]): ExtremaFit = {
    //solve Ax = b to return x in form of an ExtremaFit object
    val sz = 3
    val aug = Array.ofDim[Double](sz,sz+1) //augmented matrix for doing Gauss-Jordan
    for (i <- 0 until sz){
      for (j <- 0 until sz+1){
        if(j<sz) aug(i)(j) = aMat(i)(j)
        else aug(i)(j) = b(i)
      }
    }
    rowOp(aug(0),aug(1),0) //do the row operations. I assumed non-zero entries since we have doubles
    rowOp(aug(0),aug(2),0)
    rowOp(aug(1),aug(2),1)
    rowOp(aug(2),aug(1),2)
    rowOp(aug(2),aug(0),2)
    rowOp(aug(1),aug(0),1)

    ExtremaFit(aug(0)(sz)/aug(0)(0),aug(1)(sz)/aug(1)(1),aug(2)(sz)/aug(2)(2)) //extract x from the diagonalized matrix
  }

  //Gaussian elimination operation function
  def rowOp(rowA: Array[Double], rowB: Array[Double], index:Int): Unit = {
    val sz = rowB.size
    val factor = -rowB(index)/rowA(index)
    for(i <- 0 until sz) rowB(i) = factor*rowA(i) + rowB(i)
  }

  //for debugging
  def printMatrix(name: String, matrix: Array[Array[Double]]): Unit = {
    println(name)
    println(matrix.deep.mkString("\n"))
  }

  //multiples matrices (m x n) x (n x k)
  def matMult(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]] = {
    val m = mat1.size
    val n = mat2.size
    val k = mat2(0).size
    val ret = Array.ofDim[Double](m,k)
    for(i <- 0 until m){
      for(j <- 0 until k){
        var elem = 0.0
        for(l <- 0 until n){
          elem += mat1(i)(l)*mat2(l)(j)
        }
        ret(i)(j) = elem
      }
    }
    ret
  }

  //multiples a matrix by a vector (m x n) x (n x 1)
  def matMult(mat1: Array[Array[Double]], vect: Array[Double]): Array[Double] = {
    val m = mat1.size
    val n = vect.size
    val ret = Array.ofDim[Double](m)
    for(i <- 0 until m){
      var elem = 0.0
      for(l <- 0 until n){
        elem += mat1(i)(l)*vect(l)
      }
      ret(i) = elem
    }
    ret
  }

  // This class represents a quadratic fit to a section of the data. ax^2+bx+c
  case class ExtremaFit(a: Double, b: Double, c: Double) {
    // Solves 2ax+b = 0 to find the location of the extrema
    def location: Double = -0.5 * b / a

    def isMax = a < 0.0
  }
}