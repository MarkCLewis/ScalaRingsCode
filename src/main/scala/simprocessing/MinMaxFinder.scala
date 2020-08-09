package simprocessing

import scala.collection.mutable
import scala.math
import scala.util.Random

object MinMaxFinder {
  // //this is some test code I lazily put in here for now
  // def main(args: Array[String]): Unit = {
  //   val x = (-10.0 to 10.0 by 0.1).toSeq
  //   val y = x.map(z => 2*z*z*z*z-2*z*z*z-z*z+z)//math.cos(z*math.Pi)-0.5+(3/15)*Random.nextInt(5))
  //   //val x = Seq(1.0,2.0,3.0,4.0,5.0)
  //   //val y = Seq(1.0,5.0,6.0,5.0,1.0)
  //   val window = 5
  //   val out = useLocalExtrema(x,y,window)
  //   for(i <- 0 until out.size){
  //     if(out(i) != null) println("extreme location: "+out(i).location+", "+out(i).value)
  //   }
  // }


  //can we speed up by first looking for local extrema i.e. go along, check if each point is a local min or max for the surrounding points +/- window/2
  //then fit centered at those local extrema only??

  def getLocalExtrema(x: Seq[Double], y: Seq[Double], window: Int): Seq[Point] = {
    val allExtrema = mutable.ArrayBuffer.empty[Point]
    val lowB = window/2
    val upperB = x.size-window/2
    for(i <- lowB until upperB){
      val isMax = (y(i) == y.slice(i-lowB,i+lowB).max)
      val isMin = (y(i) == y.slice(i-lowB,i+lowB).min)
      //if(ret.size == 0 || (math.abs(y(i)-ret(ret.size-1).y) > minAmp*2 || ))
      if(isMax) allExtrema += Point(x(i),y(i),i,true)
      else if(isMin) allExtrema += Point(x(i),y(i),i,false)
    }
    //TODO MAKE THESE NOT HARD-CODED
    val globMaxX = allExtrema.find(_.y == allExtrema.map{e => e.y}.max).get.x
    println("global Max X: "+globMaxX)
    val boundedExtrema = allExtrema.filter{e => e.x > -0.0215 && ((e.x >= globMaxX && e.y > 0.06) || (e.x < globMaxX && e.y > 0.08))}//mutable.ArrayBuffer.empty[Point]

    val noDuplicates = mutable.ArrayBuffer.empty[Point]
    var cnt = 0
    while(cnt < boundedExtrema.length-1){
      val orig = cnt
      while(cnt < boundedExtrema.length-1 && boundedExtrema(cnt).isMax == boundedExtrema(cnt+1).isMax && boundedExtrema(cnt).index + window/2 >= boundedExtrema(cnt+1).index && boundedExtrema(cnt).y == boundedExtrema(cnt+1).y){
        cnt += 1
      }
      if(cnt == orig) noDuplicates += boundedExtrema(cnt)
      else{
        println("Using combining code")
        val numEqual = cnt-orig+1
        println("num equal = " + numEqual)
        var xSum = 0.0
        var ySum = 0.0
        for(j <- 0 until numEqual){
          xSum += boundedExtrema(orig+j).x
          ySum += boundedExtrema(orig+j).y
        }
        noDuplicates += Point(xSum/numEqual,ySum/numEqual,-1,boundedExtrema(orig).isMax)
      }
      cnt += 1
    }
    // var cnt = 0
    // while(cnt < allExtrema.size-1){
    //   val orig = cnt
    //   println(allExtrema(orig).index)
    //   while(cnt < allExtrema.size-1 && allExtrema(cnt).index + window/2 >= allExtrema(cnt+1).index){
    //     cnt += 1
    //   }
    //   if(cnt == orig) boundedExtrema += allExtrema(orig) 
    //   cnt += 1
    // }

    val validExtrema = noDuplicates.toSeq
    println("There are " + validExtrema.size + " valid local extrema")
    println("they are: ")
    validExtrema.foreach{pt => println("x: "+pt.x+" y: "+pt.y+" index: "+pt.index)}
    validExtrema.toSeq
  }

  def useLocalExtrema(x: Seq[Double], y: Seq[Double], window: Int): Seq[ExtremaFit] = {
    println("USING LOCAL EXTREMA")
    val ret = mutable.ArrayBuffer.empty[ExtremaFit]
    val validExtrema = getLocalExtrema(x,y,window)
    val extremaFits = Array.ofDim[ExtremaFit](validExtrema.size)

    val fittingWindows = Array.ofDim[(Int,Int)](validExtrema.size)

    for(i <- 1 until validExtrema.size-1) {
      val locExt = validExtrema(i)
      val ampLeft = (validExtrema(i-1).y - locExt.y) / 2
      val ampRight = (validExtrema(i+1).y - locExt.y) / 2
      var cntLeft = 1
      var cntRight = 1
      if(locExt.isMax){
        while(y(locExt.index+cntRight) > locExt.y + ampRight){
          cntRight += 1
        }
        while(y(locExt.index-cntLeft) > locExt.y + ampLeft){
          cntLeft += 1
        }
      }
      else {
        while(y(locExt.index+cntRight) < locExt.y + ampRight){
          cntRight += 1
        }
        while(y(locExt.index-cntLeft) < locExt.y + ampLeft){
          cntLeft += 1
        }
      }
      fittingWindows(i) = (cntLeft,cntLeft+cntRight+1)
      println("Fitting Window: "+(fittingWindows(i)))
    }
    fittingWindows(0) = fittingWindows(1)
    fittingWindows(fittingWindows.size-1) = fittingWindows(fittingWindows.size-2)
    
    val sz = 3
    var f0 = (z:Double) => z*z // f_0 = x^2
    var f1 = (z:Double) => z   // f_1 = x
    var f2 = (z:Double) => 1.0 // f_2 = 1
    val funcs = Array[(Double)=>Double](f0,f1,f2)
    for(k <- 0 until validExtrema.size){
      val locExt = validExtrema(k)
      val (windowLeft, fittingWindow) = fittingWindows(k)
      println("left: "+windowLeft+" fittingWindow: "+fittingWindow)
      val D = Array.ofDim[Double](fittingWindow,sz)
      val transD = Array.ofDim[Double](sz,fittingWindow)
      val yArr = Array.ofDim[Double](fittingWindow)
      val offset = locExt.index - windowLeft
      if(offset + fittingWindow < x.size && offset >= 0){
        for(i <- 0 until fittingWindow){ //initialize D array
          for(j <- 0 until sz){
            D(i)(j) = funcs(j)(x(i+offset))
            transD(j)(i) = D(i)(j)
            yArr(i) = y(i+offset)
          }
        }
        printMatrix("D MATRIX", D)
        printMatrix("TRANSPOSED MATRIX",transD)
        println("y Vector")
        yArr.foreach{x => print(x+", ")}
        println
        val aMat = matMult(transD,D)
        val b = matMult(transD,yArr)
        val ef = doFit(aMat,b)
        val loc = ef.location
        // If fit location isn't in the fittingWindow it is thrown out immediately.
        if(x(offset) <= loc && x(offset+fittingWindow-1) >= loc) {
          ret += ef
        }
      }
    }
    ret.toSeq
  }

  case class Point(x: Double, y:Double, index:Int, isMax:Boolean) {}

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
        var numInAvg = 1
        while (j < cnt+window && j < retArr.size){
          if (retArr(j) != null && (retArr(j).a * ef0.a) > 0){
            val ef1 = retArr(j)
            sumOfFits(0) += ef1.a
            sumOfFits(1) += ef1.b
            sumOfFits(2) += ef1.c
            numInAvg += 1
          }
          j += 1
        }
        retBuff += ExtremaFit(sumOfFits(0)/numInAvg,sumOfFits(1)/numInAvg,sumOfFits(2)/numInAvg)
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
    def value: Double = {
      val loc = location
      a*loc*loc + b*loc + c
    }
    def isMax = a < 0.0
  }
}