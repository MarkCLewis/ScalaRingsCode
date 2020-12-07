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
        val (minx,maxx) = (xValues.min,xValues.max)
        val (miny,maxy) = (yValues.min,yValues.max)
        println("Simulated Particles: " + particles.length)
        println("Particle Size (assumed uniform): " + particles(0).rad)
        println("Min X = " + minx + " Max X = " + maxx)
        println("Min Y = " + miny + " Max Y = " + maxy)
    }
}