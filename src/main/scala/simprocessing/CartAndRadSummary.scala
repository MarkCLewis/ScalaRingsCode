package simprocessing

import data.CartAndRad
import util.Particle
import util.GCCoord

import java.io.File
import scala.collection.mutable
import scala.math

import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer

object CartAndRadSummary {
	def main(args: Array[String]): Unit = {
        if (args.contains("-help") || args.length != 1) {
            println("Arguments:")
            println("The file you want to look at i.e. \\data\\lewislab\\.....\\CartAndRad.xxxxx.bin Defaults to step zero")
            sys.exit()
        }
        val particles = CartAndRad.read(new File(args(0)))
        val xValues = particles.map(_.x)
        val yValues = particles.map(_.y)
        val (minx,maxx) = (-0.02163, -0.02115)//(xValues.min,xValues.max)
        val (miny,maxy) = (yValues.min,yValues.max)
        val pRad = particles(0).rad
        println("Simulated Particles: " + particles.length)
        println("Particle Size (assumed uniform): " + pRad)
        println("Min X = " + minx + " Max X = " + maxx)
        println("Min Y = " + miny + " Max Y = " + maxy)

        //val (xBins, yBins) = (114,114)
        //plotParticleDistributions(particles.map(p => GCCoord(p)), xBins, yBins)
        val width = 10000
        val height = 10000
        val save = Some("highres.87500.png")

        val style = ScatterStyle(xValues.toSeq, yValues.toSeq, symbolWidth=2*pRad*(width/(maxx-minx)), symbolHeight=2*pRad*(height/(maxy-miny)))
        val plot = Plot.simple(style, title="Particles")
            .updatedAxis[NumericAxis]("x", _.min(minx).max(maxx).updatedName("x").numberFormat("%1.4f"))
            .updatedAxis[NumericAxis]("y", _.min(miny).max(maxy).updatedName("y").numberFormat("%1.5f"))
        //val updater = if (true) Some(SwingRenderer(plot, width, height, true)) else None
        save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix, width = width, height = height))
        
        // val plot = Plot.histogramPlotFromData(eBins, eValues, GreenARGB)
        //     .updatedAxis[NumericAxis]("x", _.updatedName("e").numberFormat("%1.6f"))
        //     .updatedAxis[NumericAxis]("y", _.updatedName("counts").numberFormat("%1.1f"))
        // val updater = if (true) Some(SwingRenderer(plot, pxSize, pxSize, true)) else None
    }

    def plotParticleDistributions(gcParticles: Seq[GCCoord], xBins: Int, yBins: Int): Unit = {
        val xValues = gcParticles.map(p => p.X)
        val yValues = gcParticles.map(p => p.Y)
        val xMin = xValues.min
        val xMax = xValues.max
        val yMin = yValues.min
        val yMax = yValues.max
        val xSpacing = (xMax-xMin)/xBins
        val ySpacing = (yMax-yMin)/yBins
        val j = yBins/2
        val divPart = Array.fill(xBins)(mutable.ArrayBuffer[GCCoord]())
        for(p <- gcParticles){
            var i = 0
            var counted = false
            while(i < xBins && !counted){
                val x0 = xMin+i*xSpacing
                val y0 = yMin+j*ySpacing
                if(p.X >= x0 && (p.X < x0+xSpacing || i == xBins-1 && p.X == x0+xSpacing) && p.Y >= y0 && (p.Y < y0+ySpacing ||
                j == yBins-1 && p.Y == y0+ySpacing)){
                    divPart(i) += p
                    counted = true
                }                
                i += 1
            }
        }
        val histBins = 10
        for(i <- 0 until xBins){
            var check = "" 
            while(check == "" || check == null) {
                println("Hit any key to continue")
                val input = scala.io.StdIn.readLine()
                check = input
            }
            val particles = divPart(i)
            val xs = particles.map(p => p.X)
            val binsX = (0 until histBins).map(index => xs.min + (xs.max-xs.min)*index/histBins)
            val ys = particles.map(p => p.Y)
            val binsY = (0 until histBins).map(index => ys.min + (ys.max-ys.min)*index/histBins)
            val es = particles.map(p => p.e)
            val binsE = (0 until histBins).map(index => es.min + (es.max-es.min)*index/histBins)
            val phis = mapAngles(particles.map(p => p.phi))
            val binsPhi = (0 until histBins).map(index => phis.min + (phis.max-phis.min)*index/histBins)
            val is = particles.map(p => p.i)
            val binsI = (0 until histBins).map(index => is.min + (is.max-is.min)*index/histBins)
            val zetas = mapAngles(particles.map(p => p.zeta))
            val binsZeta = (0 until histBins).map(index => zetas.min + (zetas.max-zetas.min)*index/histBins)

            val pxSize = 1000
            val plot = Plot.histogramPlotFromData(binsX, xs, GreenARGB,title="X: "+i+" of "+xBins)
            .updatedAxis[NumericAxis]("x", _.updatedName("X").numberFormat("%1.6f"))
            .updatedAxis[NumericAxis]("y", _.updatedName("counts").numberFormat("%1.1f"))
            val updater = if (true) Some(SwingRenderer(plot, pxSize, pxSize, true)) else None
        }
        
    }
    def mapAngles(oldAngles: Seq[Double]): Seq[Double] = {
        if(oldAngles.length > 0){
            val newAngles = Array.ofDim[Double](oldAngles.length)
            val first = oldAngles(0)
            newAngles(0) = first
            for(i <- 1 until oldAngles.length){
                var mod = oldAngles(i)
                while (mod < first - math.Pi) mod += 2*math.Pi
                while (mod > first + math.Pi) mod -= 2*math.Pi
                newAngles(i) = mod
            }
            newAngles
        }
        else{
            oldAngles
        } 
    }
}