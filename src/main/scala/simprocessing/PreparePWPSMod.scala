package simprocessing

import data.CartAndRad
import util.Particle

import scala.math

object PreparePWPSMod {
	def main(args: Array[String]): Unit = {
        // if (args.contains("-help") || args.length < 1) {
        //     println("Arguments:")
        //     println("\t-dir path: path to directory, defaults to current directory")
        //     println("\t-step: the step you want to base the new file off of. Defaults to 0")
        //     println("\t-width #: width of window/image in pixels, defaults to 1000")
        //     println("\t-height #: height of window/image in pixels, defaults to 1000")
        //     println("\t-display: tells if the image should be displayed in a window")
        //     println("\t-save prefix: tells if images should be saved and gives prefix")
        //     println("\t-azMin #:the minimum azimuthal value to display in the surface plot")
        //     println("\t-azMax #:the maximum azimuthal value to display in the surface plot")
        //     sys.exit()
        // }
        // val dir = new File(args.sliding(2).find(_(0) == "-dir").map(_(1)).getOrElse("."))
        // val step = args.sliding(2).find(_(0) == "-stepRange").map(_(1)).getOrElse(0)
        // val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
        // val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
        // val display = args.contains("-display")
        // val save = args.sliding(2).find(_(0) == "-save").map(_(1))
        // val azMin = args.sliding(2).find(_(0) == "-azMin").map(_(1).toDouble).getOrElse(Double.MinValue)
        // val azMax = args.sliding(2).find(_(0) == "-azMax").map(_(1).toDouble).getOrElse(Double.MaxValue)

        // val particles = CartAndRad.read(new File(dir, "CartAndRad."+step+".bin"))

        // //modify the particles in some way
        // //return the modified particles
        // val newParticles = particles

        // CartAndRad.write(new File(dir, "CartAndRadMod."+step+".bin"), newParticles)
        val p = Particle(0,0,0,0,0,0,1)
        twoSizeCubeExpand(p)
    }

    def twoSizeCubeExpand(p: Particle): Seq[Particle] = {
        val newParticles = Array.ofDim[Particle](11)
        val r0 = p.rad

        for(i <- 0 until newParticles.length){
            if(i < 4){
                newParticles(i) = Particle(0.0,0.0,0.0,0.0,0.0,0.0,0.5 * r0)
            }
            else{
                newParticles(i) = Particle(0.0,0.0,0.0,0.0,0.0,0.0,0.25 * r0)
            }
        }
        println(getVolume(p))
        println(newParticles.map(np => getVolume(np)).sum)
        newParticles.toSeq
    }

    def getVolume(p:Particle): Double = (4.0/3) * math.Pi * math.pow(p.rad,3)
}