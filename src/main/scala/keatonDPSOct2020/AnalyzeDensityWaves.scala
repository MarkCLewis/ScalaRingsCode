package keatonDPSOct2020

import data.FixedBinned
import simprocessing.MinMaxFinder

import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer

import java.io._ 
import scala.collection.mutable

object AnalyzeDensityWaves {
    val SIGMA = 450 //kg/m2
    val RL = -2/(3*31.0) //dimensionless. Multiply by 139380km to get actual radius
    val RP = 139380 //km
    val G = 6.67430e-11 //m3/kgs2
    val moonPeriod = 52963.2 //seconds
    val mNumber = 32
	def main(args: Array[String]): Unit = {
        if (args.contains("-help") || args.length < 1) {
            println("Arguments:")
            println("\t-dir path: path to directory, defaults to current directory")
            println("\t-width #: width of window/image in pixels, defaults to 1000")
            println("\t-height #: height of window/image in pixels, defaults to 1000")
            println("\t-display: tells if the image should be displayed in a window")
            println("\t-save prefix: tells if images should be saved and gives prefix")
            println("\t-stepRange #: the inclusive range of steps you want to analyze as 2 integers. Defaults to the whole file")
            println("\t-bigPlot #: step to start compiling wavenumbers")
            println("\t-azMin #:the minimum azimuthal value to display in the surface plot")
            println("\t-azMax #:the maximum azimuthal value to display in the surface plot")
            sys.exit()
        }
        val dir = new File(args.sliding(2).find(_(0) == "-dir").map(_(1)).getOrElse("."))
        val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
        val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
        val display = args.contains("-display")
        val save = args.sliding(2).find(_(0) == "-save").map(_(1))
        val stepRange = args.sliding(3).find(_(0) == "-stepRange").map(arg => (arg(1).toInt/100, arg(2).toInt/100)).getOrElse((-1,0))
        val bigPlotStart = args.sliding(2).find(_(0) == "-bigPlot").map(_(1).toInt).getOrElse(-1)
        val azMin = args.sliding(2).find(_(0) == "-azMin").map(_(1).toDouble).getOrElse(Double.MinValue)
        val azMax = args.sliding(2).find(_(0) == "-azMax").map(_(1).toDouble).getOrElse(Double.MaxValue)

        val (categories, allFixedBins) = FixedBinned.read(new File(dir, "FixedBinned.bin").getAbsolutePath())
        val sgn = math.signum(allFixedBins(1)(0)(0) - allFixedBins(0)(0)(0))
        val fixedBins = allFixedBins.filter(col => col(0)(0) >= azMin && col(0)(0) <= azMax)
        if(true) categories.zipWithIndex.foreach(println)

        println("stepRange",stepRange)
        println("bigplotstart",bigPlotStart)
        println("FixedBins Length: " + fixedBins.length)

        val lowFracDist = 0.0
        val highFracDist = 0.015

        if (bigPlotStart == -1){
            val (start, end) = stepRange
            for ((step, index) <- fixedBins.slice(start,end+1).zipWithIndex) {
                val yValues = step.map { col => col(1) } //these are really the radial coordinates
                val tauValues = step.map { col => col(4) }
                val window = 20
                val extrema = MinMaxFinder.getLocalExtrema(yValues,tauValues,window,true).filter(_.isMax)
                val locations = extrema.map { e => e.x}
                val extremeValues = extrema.map { e => e.y}
                
                val radialValues = Array.ofDim[Double](locations.length-1)
                val wavenumber = Array.ofDim[Double](locations.length-1)
                for(i <- 0 until locations.length-1){
                    radialValues(i) = ((locations(i+1)+locations(i))/2 - RL)/(-RL)
                    wavenumber(i) = 2*math.Pi/((locations(i+1)-locations(i))*RP)
                }
                val (slope, intercept) = doLinearFit(radialValues, wavenumber)
                //val slope = (3*(mNumber-1)*2*math.Pi) / (G*450*math.pow((moonPeriod*(mNumber-1))/(mNumber),2))
                //val intercept = findBestIntercept(radialValues,wavenumber,slope)
                println("slope",slope,"intercept",intercept)
                val fitX = Seq(lowFracDist, highFracDist)
                val fitY = fitX.map(x => slope*x + intercept)

                val printIndex = 100*(start+index)

                val tauStyle = ScatterStyle(yValues, tauValues, symbolWidth=5, symbolHeight=5)
                val extremaStyle = ScatterStyle(locations, extremeValues, symbolWidth=10, symbolHeight=10, colors=RedARGB)
                val lambdaStyle = ScatterStyle(radialValues, wavenumber, symbolWidth=10, symbolHeight=10)
                val fitStyle = ScatterStyle(fitX, fitY, symbolWidth=5, symbolHeight=5, colors=RedARGB,
                    lines = Some(ScatterStyle.LineData(0, Renderer.StrokeData(1,Nil))))

                if(save.nonEmpty || display){
                    val topRow = Seq(Seq(tauStyle, extremaStyle), Seq(lambdaStyle, fitStyle))
                    val bottomRow = Seq()
                    val styles = Seq(topRow)
                    val grid = Plot.stackedGridNN(styles)
                    .withModifiedAxis[NumericAxis]("y","y2",_.min(0.0).max(2.5)
                        .updatedName("Wavenumber").maxSide.numberFormat("%1.3f"))
                    .updatedStyleYAxis("y2", 0, 1, 0)
                    .updatedStyleYAxis("y2", 0, 1, 1)
                    .withModifiedAxis[NumericAxis]("x","x2",_.min(lowFracDist).max(highFracDist)
                        .updatedName("Fractional Dist. From Resonance").numberFormat("%1.3f"))
                    .updatedStyleXAxis("x2", 0, 1, 0)
                    .updatedStyleXAxis("x2", 0, 1, 1)
                    //     .updatedName("Radial").spacing(1e-4))
                    .updatedAxis[NumericAxis]("x", _.updatedName("Radial").numberFormat("%1.4f"))
                    .updatedAxis[NumericAxis]("y", _.min(0.0).max(0.25).updatedName("Optical Depth").numberFormat("%1.4f"))

                    val updaterGrid = if (display) Some(SwingRenderer(grid, width, height, true)) else None
                    save.foreach(prefix => SwingRenderer.saveToImage(grid, prefix + s".$printIndex.png", width = width, height = height))
                }
            }
        }
        else {
            val (start, end) = (bigPlotStart/100, fixedBins.length-1)
            val allRadii = mutable.ArrayBuffer[Double]()
            val allWavenumbers = mutable.ArrayBuffer[Double]()
            for ((step, index) <- fixedBins.slice(start,end+1).zipWithIndex) {
                val yValues = step.map { col => col(1) } //these are really the radial coordinates
                val tauValues = step.map { col => col(4) }
                val window = 20
                val extrema = MinMaxFinder.getLocalExtrema(yValues,tauValues,window,true).filter(_.isMax)
                val locations = extrema.map { e => e.x}
                val extremeValues = extrema.map { e => e.y}
                
                for(i <- 0 until locations.length-1){
                    val r = ((locations(i+1)+locations(i))/2 - RL)/(-RL)
                    val k = 2*math.Pi/((locations(i+1)-locations(i))*RP)
                    if(0.002 < r && k < 3.0){
                        allRadii += r
                        allWavenumbers += k
                    }
                }
                println("radii now has", allRadii.length,"elements")
            }
            val bigSlope = doPropFit(allRadii,allWavenumbers)
            println("Slope on k(x) = " + bigSlope)
            println("Gives " + getLocalDensity(bigSlope))

            val kStyle = ScatterStyle(allRadii, allWavenumbers, symbolWidth=5, symbolHeight=5)
            val kPlotBig = Plot.simple(kStyle,xLabel="Fractional Distance From Resonance",yLabel="Wavenumber (1/km)")
            .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f")))
                .min(lowFracDist).max(highFracDist))
            .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f")))
                .min(0.0).max(2.5))
            save.foreach(prefix => SwingRenderer.saveToImage(kPlotBig, prefix + ".png", width = width, height = height))
            val updaterK = if (true) Some(SwingRenderer(kPlotBig, width, height, true)) else None
            println("Done drawing Big Plot")

            val allDensities = (allRadii.toSeq, allWavenumbers.toSeq).zipped.map{(r,k) => getLocalDensity(k/r)}
            val sStyle = ScatterStyle(allRadii.toSeq, allDensities, symbolWidth=5, symbolHeight=5)//, colors=RedARGB,
                //lines = Some(ScatterStyle.LineData(0, Renderer.StrokeData(1,Nil))))
            val sigmaPlotBig = Plot.simple(sStyle,xLabel="Fractional Distance From Resonance",yLabel="Surface Density (kg/m^2)")
            .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f")))
                )//.min(lowFracDist).max(highFracDist))
            .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f")))
                )//.min(0.0).max(3.0))
            save.foreach(prefix => SwingRenderer.saveToImage(sigmaPlotBig, prefix + ".png", width = width, height = height))
            val updaterSigma = if (true) Some(SwingRenderer(sigmaPlotBig, width, height, true)) else None
            println("Done drawing Density Plot")
        }
        
	}

    def getLocalDensity(slope: Double): Double = {
        val numerator = 3*(mNumber-1)
        val sqr = (mNumber-1)*moonPeriod/mNumber
        val denominator = 2*math.Pi*G*sqr*sqr*slope
        numerator/denominator
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

    def findBestIntercept(x: Seq[Double], y: Seq[Double], slope:Double): Double = {
        val diff = (for(i <- 0 until x.length) yield {
            y(i) - slope*x(i)
        }).toSeq
        diff.sum/diff.length
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
}