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
        val eValues = particles.map(p => GCCoord(p).e)
        val eBins = (0 until 100).map(i => eValues.min + i*(eValues.max-eValues.min)/100.0)
        val (minx,maxx) = (xValues.min,xValues.max)
        val (miny,maxy) = (yValues.min,yValues.max)
        val pRad = particles(0).rad
        println("Simulated Particles: " + particles.length)
        println("Particle Size (assumed uniform): " + pRad)
        println("Min X = " + minx + " Max X = " + maxx)
        println("Min Y = " + miny + " Max Y = " + maxy)

        val pxSize = 1000
        // val style = ScatterStyle(xValues.toSeq, yValues.toSeq, symbolWidth=2*pRad*(pxSize/(maxx-minx)), symbolHeight=2*pRad*(pxSize/(maxy-miny)))
        // val plot = Plot.simple(style, title="Particles")
        //     .updatedAxis[NumericAxis]("x", _.min(minx).max(maxx).updatedName("x").numberFormat("%1.4f"))
        //     .updatedAxis[NumericAxis]("y", _.min(miny).max(maxy).updatedName("y").numberFormat("%1.5f"))
        // val updater = if (true) Some(SwingRenderer(plot, pxSize, pxSize, true)) else None
        
        //val style = HistogramStyle(eBins, Seq(HistogramStyle.DataAndColor(0.0,BlackARGB)))
        val plot = Plot.histogramPlotFromData(eBins, eValues, GreenARGB)
            .updatedAxis[NumericAxis]("x", _.updatedName("e").numberFormat("%1.6f"))
            .updatedAxis[NumericAxis]("y", _.updatedName("counts").numberFormat("%1.1f"))
        val updater = if (true) Some(SwingRenderer(plot, pxSize, pxSize, true)) else None
    }

    def binForHist(data: Seq[Double], n:Int):Seq[Int] = {
        val ret = Array.ofDim[Int](n)
        val min = data.min
        val max = data.max
        val binStep = (max-min)/n
        for(d <- data){
            var index = math.round(((d-min) - (d-min) % binStep)/binStep).toInt
            index = if(index < 0) 0 else if(index > n-1) n-1 else index
            ret(index) += 1
        }
        ret
    }
}