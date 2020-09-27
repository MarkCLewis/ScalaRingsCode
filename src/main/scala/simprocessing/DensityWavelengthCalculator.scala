package simprocessing

import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer

import scala.io.Source

// reads in a text file of following format:
// # of different slices (plots of tau as function of radius) 1 LINE
// title or simulation step number
// number of maxima (N)
// radial positions of the maxima... (N lines)
// title or simulation step number
// number of maxima (N)
// radial positions of the maxima... (N lines) ......

// then calculates the wavelength between peaks as a function of the radius and plots
object DensityWavelengthCalculator {
	def main(args: Array[String]): Unit = {
        if (args.contains("-help") || args.length < 1) {
				println("Arguments:")
				println("\t-name: file name to read from")
				sys.exit()
			}
        val fileName = args.sliding(2).find(_(0) == "-name").map(_(1)).getOrElse("testFile.txt")
        val data = readMaximaLocs(fileName)
        val SIGMA = 45
        val R0 = 2/(3*31.0)
        for((step, maxima) <- data){
            val radialValues = Array.ofDim[Double](maxima.length-1)
            val lambdaValues = Array.ofDim[Double](maxima.length-1)
            for(i <- 0 until maxima.length-1){
                radialValues(i) = (maxima(i+1)+maxima(i))/2 - R0
                lambdaValues(i) = maxima(i+1)-maxima(i)
            }
            val (slope, intercept) = doLinearFit(radialValues.map(r => SIGMA/(R0-r)), lambdaValues)
            //val slope = doPropFit(radialValues.map(r => 1/(R0-r)), lambdaValues)
            val (start, end) = (radialValues(0), radialValues(radialValues.length-1))
            val fitX = (start to end by (end-start)/20).toSeq
            val fitY = fitX.map(x => slope*(SIGMA/(R0-x)) + intercept)
            //val fitY = fitX.map(x => slope*(1/(R0-x)))

            val (width, height) = (1000, 1000)
            val display = true
            val plot = Plot.scatterPlots(Seq((radialValues,lambdaValues,BlackARGB,10),(fitX,fitY,RedARGB,5)),title=("Step number "+step),xLabel="radial",yLabel="wavelength")
            .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.5f"))))
            .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.5f"))))
            val updater = if (display) Some(SwingRenderer(plot, width, height, true)) else None
            println("Done drawing Step # " + step)

        }
    }

    //takes a set of points and returns a pair of coordinates as Seq(x0,y0,x1,y1)
    def doLinearFit(x: Seq[Double], y: Seq[Double]): (Double, Double) = {
        val sz = 2
        var f1 = (z:Double) => z   // f_1 = x
        var f2 = (z:Double) => 1.0 // f_2 = 1
        val funcs = Array[(Double)=>Double](f1,f2)
        val D = Array.ofDim[Double](x.length, sz)
        val transD = Array.ofDim[Double](sz,x.length)
        for(i <- 0 until x.length){ //initialize D array
          for(j <- 0 until sz){
            D(i)(j) = funcs(j)(x(i))
            transD(j)(i) = D(i)(j)
          }
        }
        val aMat = MinMaxFinder.matMult(transD,D)
        val b = MinMaxFinder.matMult(transD,y.toArray)
        val aug = Array.ofDim[Double](sz,sz+1) //augmented matrix for doing Gauss-Jordan
        for (i <- 0 until sz){
            for (j <- 0 until sz+1){
                if(j<sz) aug(i)(j) = aMat(i)(j)
                else aug(i)(j) = b(i)
            }
        }
        MinMaxFinder.rowOp(aug(0),aug(1),0) //do the row operations. I assumed non-zero entries since we have doubles
        MinMaxFinder.rowOp(aug(1),aug(0),1)

        (aug(0)(sz)/aug(0)(0), aug(1)(sz)/aug(1)(1)) //extract x from the diagonalized matrix
    }

    def doPropFit(x: Seq[Double], y: Seq[Double]): Double = {
        val sz = 1
        var f = (z:Double) => z   // f = x
        val D = Array.ofDim[Double](x.length)
        for(i <- 0 until x.length){ //initialize D vector
            D(i) = f(x(i))
        }
        val a = D.map(d => d*d).sum
        val b = dot(D,y.toArray)

        b/a //extract x from the diagonalized matrix
    }

    def dot(v1: Array[Double], v2: Array[Double]): Double = {
        var ret = 0.0
        for(i <- 0 until v1.length){
            ret += v1(i)*v2(i)
        }
        ret
    }

    def readMaximaLocs(filename: String): Seq[(Int, Seq[Double])] = {
		val bufferedSource = Source.fromFile(new java.io.File(filename))
		val lines = bufferedSource.getLines()
		val numSteps = lines.next.toInt
		val ret = Array.ofDim[(Int, Seq[Double])](numSteps)
		for(i <- 0 until numSteps){
            val step = lines.next.toInt
			val numMaxima = lines.next.toInt
			val maxima = Array.ofDim[Double](numMaxima)
			for(j <- 0 until numMaxima){
				maxima(j) = lines.next.toDouble
			}
			val pair = (step, maxima.toSeq)
			ret(i) = pair
		}
		bufferedSource.close
		ret.toSeq
	}
}