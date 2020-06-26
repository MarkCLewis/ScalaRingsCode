package simprocessing

import scala.collection.mutable
import scala.math

object MinMaxFinder {
  //this is some test code I lazily put in here for now
  // def main(args: Array[String]): Unit = {
  //   val x = (-0.1 to 10.1 by 0.1).toSeq
  //   val y = x.map(z => z*z - 2*z + 4)//(z => math.cos(z*math.Pi))
  //   //val x = Seq(1.0,2.0,3.0,4.0,5.0)
  //   //val y = Seq(1.0,5.0,6.0,5.0,1.0)
  //   val window = 3
  //   val out = apply(x,y,window)
  //   println("number of valid extrema: " + out.filter(_!=null).size)
  //   for(i <- 0 until out.size){
  //     if(out(i) != null) println("extreme location:",out(i).location)
  //   }
  // }

  def apply(x: Seq[Double], y: Seq[Double], window: Int): Seq[ExtremaFit] = {
    val retArr = Array.ofDim[ExtremaFit](x.size-window+1)
    val sz = 3
    
    var f0 = (z:Double) => z*z // f_0 = x^2
    var f1 = (z:Double) => z   // f_1 = x
    var f2 = (z:Double) => 1.0 // f_2 = 1
    val funcs = Array[(Double)=>Double](f0,f1,f2)
    //     |f_0(x_0) f_1(x_0) f_2(x_0)|
    // D = |f_0(x_1) f_1(x_1) f_2(x_1)|
    //     |...      ...      ...     |
    //     |f_0(x_n) f_1(x_n) f_2(x_n)|
    // Ax=b, A = D^T*D, b = D^T*y
    // Do running quadratic fit. D^T*D and D^T*y can be calculated in a running way by adding and removing a row from D and y.
    val D = mutable.ArrayBuffer.fill(window,sz)(0.0)
    val transD = Array.ofDim[Double](sz,window)
    val yBuff = y.slice(0,window).toBuffer

    for(i <- 0 until window){ //initialize D arraybuffer
      for(j <- 0 until sz){
        D(i)(j) = funcs(j)(x(i))
      }
    }

    for(slider <- 0 until x.size+1-window){
      val last = slider+window-1
      if(slider > 0){ //we need to modify D and yArr
        yBuff.remove(0)
        yBuff.append(y(last))
        D.remove(0) //remove the top row
        D.append(mutable.ArrayBuffer.fill(sz)(0.0)) //add a new (empty) bottom row
        for(j <- 0 until sz){
          D(window-1)(j) = funcs(j)(x(last)) //fill in the new bottom row
        }
      }
      for(i <- 0 until window){ //update the transpose
        for(j <- 0 until sz){
          transD(j)(i) = D(i)(j)
        }
      }
      val aMat = matMult(transD,D.map(_.toArray).toArray)
      val b = matMult(transD,yBuff.toArray)
      val ef = doFit(aMat,b)
      val loc = ef.location
      // If fit location isn't in the window it is thrown out immediately.
      if(x(slider) <= loc && x(last) >= loc) {
        retArr(slider) = ef
      } else {
        retArr(slider) = null
      }
    }

    //println(retArr.deep.mkString("\n"))

    // For remaining, fits need to find those nearby one another and somehow combine the ones that agree. This could just be a fit with a larger window.
    //I have done this by summing the a,b,c elements of each set of consecutive ExtremaFit and dividing by the number of consecutive valid fits
    val retBuff = mutable.ArrayBuffer.empty[ExtremaFit]
    
    var cnt = 0
    while (cnt < retArr.size){
      if (retArr(cnt) != null){
        val ef0 = retArr(cnt)
        val sumOfFits = Array(ef0.a,ef0.b,ef0.c)
        var j = cnt+1
        while (j < retArr.size && retArr(j) != null && (retArr(j).a * ef0.a) > 0){
          val ef1 = retArr(j)
          sumOfFits(0) += ef1.a
          sumOfFits(1) += ef1.b
          sumOfFits(2) += ef1.c
          j += 1
        }
        val range = j - cnt
        retBuff += ExtremaFit(sumOfFits(0)/range,sumOfFits(1)/range,sumOfFits(2)/range)
        cnt = j
      }
      else cnt += 1
    }
    
    retBuff.toSeq
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

  //multiplies matrices (m x n) x (n x k)
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

  //multiplies a matrix by a vector (m x n) x (n x 1)
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