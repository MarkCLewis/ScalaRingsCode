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
import scala.collection.immutable.ArraySeq

object CartAndRadSummary {
	def main(args: Array[String]): Unit = {
        if (args.contains("-help") || (args.length != 1 && args.length != 3 && args.length != 4)) {
            println("Arguments:")
            println("The file you want to look at i.e. \\data\\lewislab\\.....\\CartAndRad.xxxxx.bin Defaults to step zero")
            println("-save name: The save name for a png plot of the particles")
            println("thesis - add if you want to make it 1000x1000 instead of 10kx10k")
            sys.exit()
        }
        val particles = CartAndRad.read(new File(args(0)))
        val save = args.sliding(2).find(_(0) == "-save").map(_(1))
        val thesis = args.sliding(1).find(_(0) == "thesis").map(_(0)).getOrElse("")
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
        val width = if(thesis == "thesis") 1000 else 10000
        val height = width

        val style = ScatterStyle(xValues.map(x => kmScale(x)).toSeq, yValues.toSeq, symbolWidth=139380*pRad*2, symbolHeight=2*pRad, xSizing=PlotSymbol.Sizing.Scaled, ySizing=PlotSymbol.Sizing.Scaled)
        val plot = Plot.simple(style)
            .updatedAxis[NumericAxis]("x", _.min(kmScale(minx)).max(kmScale(maxx)).updatedName("Radial Distance From Resonance [km]").numberFormat("%1.0f"))
            .updatedAxis[NumericAxis]("y", _.min(miny).max(maxy).updatedName("Azimuthal [R0]").numberFormat("%1.5f"))
        //val updater = if (true) Some(SwingRenderer(plot, width, height, true)) else None
        save.foreach(prefix => SwingRenderer.saveToImage(plot, (prefix+".png"), width = width, height = height))
        
        // val plot = Plot.histogramPlotFromData(eBins, eValues, GreenARGB)
        //     .updatedAxis[NumericAxis]("x", _.updatedName("e").numberFormat("%1.6f"))
        //     .updatedAxis[NumericAxis]("y", _.updatedName("counts").numberFormat("%1.1f"))
        // val updater = if (true) Some(SwingRenderer(plot, pxSize, pxSize, true)) else None
    }

    def kmScale(v: Double): Double = (v+2.0/(3*31))*139380

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
            val phis = mapAngles(particles.map(p => p.phi).toSeq)
            val binsPhi = (0 until histBins).map(index => phis.min + (phis.max-phis.min)*index/histBins)
            val is = particles.map(p => p.i)
            val binsI = (0 until histBins).map(index => is.min + (is.max-is.min)*index/histBins)
            val zetas = mapAngles(particles.map(p => p.zeta).toSeq)
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
            ArraySeq.unsafeWrapArray(newAngles)
        }
        else{
            oldAngles
        } 
    }
}