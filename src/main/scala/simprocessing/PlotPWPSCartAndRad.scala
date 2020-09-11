package simprocessing

import data.CartAndRad
import util.Particle

import java.io.File
import scala.collection.mutable
import scala.math

import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer

object PlotPWPSCartAndRad {
    val MOON_ORBIT_RADIUS = 139380
    val PARTICLE_RADIUS = 0.00000006 //6e-8
    val CELL_WIDTH = 4*15.0/MOON_ORBIT_RADIUS

	def main(args: Array[String]): Unit = {
        if (args.contains("-help") || args.length < 1) {
            println("Arguments:")
            println("\t-dir path: path to directory, defaults to current directory")
            println("\t-bin numBins: set the number of bins on each axis (if unfilled defaults to a scatterplot of all particles")
            println("\t-width #: width of window/image in pixels, defaults to 1000")
            println("\t-height #: height of window/image in pixels, defaults to 1000")
            println("\t-display: tells if the image should be displayed in a window")
            println("\t-save prefix: tells if images should be saved and gives prefix")
            println("\t-stepRange #: the (inclusive) range of step numbers you want to look at i.e. CartAndRad.xxxxx.bin - CartAndRad.yyyyy.bin. Defaults to step zero")
            println("\t-azMin #:the minimum azimuthal value to display in the surface plot")
            println("\t-azMax #:the maximum azimuthal value to display in the surface plot")
            sys.exit()
        }
        val dir = new File(args.sliding(2).find(_(0) == "-dir").map(_(1)).getOrElse("."))
        val binCount = args.sliding(2).find(_(0) == "-bin").map(_(1).toInt)
        val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
        val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
        val display = args.contains("-display")
        val save = args.sliding(2).find(_(0) == "-save").map(_(1))
        val (start,end) = args.sliding(3).find(_(0) == "-stepRange").map(arg => (arg(1).toInt, arg(2).toInt)).getOrElse(0,0)
        val azMin = args.sliding(2).find(_(0) == "-azMin").map(_(1).toDouble).getOrElse(Double.MinValue)
        val azMax = args.sliding(2).find(_(0) == "-azMax").map(_(1).toDouble).getOrElse(Double.MaxValue)

        for(step <- start to end by 100){
            val particles = CartAndRad.read(new File(dir, "CartAndRad."+step+".bin"))
            val xValues = particles.map(_.x)
            val yValues = particles.map(_.y)

            val foo = Seq(xValues.min,xValues.max)
            val bar = Seq(yValues.min,yValues.max)

            if(save.nonEmpty || display){
                if(binCount.nonEmpty){
                    val numBins = binCount.getOrElse(100)
                    val (binX, binY, coveredArea) = getSurfDensity(particles,numBins)
                    val gradient = ColorGradient(coveredArea.min -> BlueARGB, coveredArea.max -> GreenARGB)

                    val plot = Plot.scatterPlots(Seq((binX,binY,gradient(coveredArea),width/numBins),(foo,bar,RedARGB,0)),title=("Step number "+step),xLabel="radial",yLabel="azimuthal")
                    .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                    .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f"))))
                    save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix + s".$step.png", width = width, height = height))
                    println("Done drawing Step # " + step)

                    val updater = if (display) Some(SwingRenderer(plot, width, height, true)) else None
                    println("Done drawing Step # " + step)
                }
                else{
                    val pixelSizes = particles.map(_.rad*2*width/CELL_WIDTH)

                    println("px size",pixelSizes(0))

                    val plot = Plot.scatterPlots(Seq((xValues,yValues,BlackARGB,pixelSizes),(foo,bar,RedARGB,0)),title=("Step number "+step),xLabel="radial",yLabel="azimuthal")
                    .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                    .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f"))))
                    save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix + s".$step.png", width = width, height = height))
                    println("Done drawing Step # " + step)

                    val updater = if (display) Some(SwingRenderer(plot, width, height, true)) else None
                    println("Done drawing Step # " + step)
                }
            }
        }
        // val x = Seq(0.0,1.0,2.0,3.0,1.0)
        // val y = Seq(3.0,4.0,6.0,8.0,6.0)
        // val foo = Seq(x.min,x.max)
        // val bar = Seq(y.min,y.max)
        // val numBins = 3
        // val particles = for(i <- 0 until x.length) yield {Particle(x(i),y(i),0.0,0.0,0.0,0.0,0.0)}
        // val (xb, yb, cts) = getDensity(particles,numBins)
        // val plot = Plot.scatterPlots(Seq((xb,yb,ColorGradient(0.0 -> BlackARGB, cts.max/2 -> BlueARGB, cts.max -> GreenARGB)(cts),20),(foo,bar,RedARGB,1)))
        // (xb,yb,ColorGradient(0.0 -> BlackARGB, cts.max/2 -> BlueARGB, cts.max -> GreenARGB)(cts),2)
        // SwingRenderer(plot, 1000, 1000, true)

        // for(i <- 0 until numBins*numBins){
        //     println(xb(i),yb(i),cts(i))
        // }
    }

    def getSurfDensity(particles: IndexedSeq[Particle], numBins: Int): (Array[Double], Array[Double], Array[Double]) = {
        val xValues = particles.map(_.x)
        val yValues = particles.map(_.y)
        val xMin = xValues.min
        val xMax = xValues.max
        val yMin = yValues.min
        val yMax = yValues.max
        val xSpacing = (xMax-xMin)/numBins
        val ySpacing = (yMax-yMin)/numBins

        val xBins = Array.ofDim[Double](numBins*numBins)//(xMin + xSpacing/2 to xMax - xSpacing/2 by xSpacing).toSeq
        val yBins = Array.ofDim[Double](numBins*numBins)//(yMin + ySpacing/2 to yMax - ySpacing/2 by ySpacing).toSeq
        for(i <- 0 until numBins){
            for(j <- 0 until numBins){
                val x0 = xMin+i*xSpacing
                val y0 = yMin+j*ySpacing
                xBins(i*numBins+j) = x0+xSpacing/2
                yBins(i*numBins+j) = y0+ySpacing/2
            }
        }

        val coveredAreas = Array.fill[Double](numBins*numBins)(0.0)
        for(p <- particles){
            var i = 0
            var counted = false
            while(i < numBins && !counted){
                var j = 0
                while(j < numBins && !counted){
                    val x0 = xMin+i*xSpacing
                    val y0 = yMin+j*ySpacing
                    if(p.x >= x0 && (p.x < x0+xSpacing || i == numBins-1 && p.x == x0+xSpacing) && p.y >= y0 && (p.y < y0+ySpacing || j == numBins-1 && p.y == y0+ySpacing)){
                        coveredAreas(i*numBins+j) += math.Pi * math.pow(p.rad,2)
                        counted = true
                    }
                    j += 1
                }
                i += 1
            }
        }
        (xBins, yBins, coveredAreas)
    }
}