package simprocessing

import data.CartAndRad
import util.Particle
import util.GCCoord

import scala.math
import scala.collection.mutable

object PreparePWPSMod {
	def main(args: Array[String]): Unit = {
        val TESTING = true
        if(TESTING){
            val xs = Seq(0, 3, 6.5, 7, 10)
            val ys = Seq(0, 1, 5, 5, 10)
            val ps = (0 to 4 by 1).map(i => GCCoord(xs(i),ys(i),0,0,0,0,1))
            val cells = getCellStats(ps,1,2,1)
            cells.foreach(l => l.foreach(c => println(c.volume())))
            sys.exit()
        }
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

        // read in the particles
        //val particles = CartAndRad.read(new File(dir, "CartAndRad."+step+".bin"))
        
        // //modify the particles in some way
        

        // return the modified particles
        // CartAndRad.write(new File(dir, "CartAndRadMod."+step+".bin"), newParticles)
    }

    def getCellStats(gcParticles: Seq[GCCoord], xBins: Int, yBins: Int, pMass: Double): Array[Array[Cell]] = {
        val xValues = gcParticles.map(p => p.X)
        val yValues = gcParticles.map(p => p.Y)
        val xMin = xValues.min
        val xMax = xValues.max
        val yMin = yValues.min
        val yMax = yValues.max
        val xSpacing = (xMax-xMin)/xBins
        val ySpacing = (yMax-yMin)/yBins

        val divPart = Array.fill(xBins,yBins)(mutable.ArrayBuffer[GCCoord]())
        for(p <- gcParticles){
            var i = 0
            var counted = false
            while(i < xBins && !counted){
                var j = 0
                while(j < yBins && !counted){
                    val x0 = xMin+i*xSpacing
                    val y0 = yMin+j*ySpacing
                    if(p.X >= x0 && (p.X < x0+xSpacing || i == xBins-1 && p.X == x0+xSpacing) && p.Y >= y0 && (p.Y < y0+ySpacing ||
                    j == yBins-1 && p.Y == y0+ySpacing)){
                        divPart(i)(j) += p
                        counted = true
                    }
                    j += 1
                }
                i += 1
            }
        }
        //for(i <- 0 until xBins){ for(j <- 0 until yBins){ println(divPart(i)(j).length)} }

        val grid = Array.ofDim[Cell](xBins,yBins)
        for(i <- 0 until xBins){
            for(j <- 0 until yBins){
                val x0 = xMin + i*xSpacing
                val y0 = yMin + j*ySpacing
                val cellParticles = divPart(i)(j)
                val pRad = cellParticles(0).rad
                val X = meanAndVariance(cellParticles.map(p => p.X))
                val Y = meanAndVariance(cellParticles.map(p => p.Y))
                val e = meanAndVariance(cellParticles.map(p => p.e))
                val phi = meanAndVariance(cellParticles.map(p => p.phi))
                val I = meanAndVariance(cellParticles.map(p => p.i))
                val zeta = meanAndVariance(cellParticles.map(p => p.zeta))
                grid(i)(j) = Cell(x0,y0,xSpacing,ySpacing,cellParticles.length,pRad,pMass,X,Y,e,phi,I,zeta)
            }
        }

        grid
    }

    def meanAndVariance(set: Seq[Double]): (Double, Double) = {
        val mean = set.sum/set.length
        val variance = set.map(x => x*x).sum/set.length - mean*mean
        (mean, variance)
    }
    
    case class Cell(x0: Double, y0: Double, xSz:Double, ySz: Double, pcnt: Int, pRad: Double, pMass: Double, X: (Double, Double), 
        Y: (Double, Double), e: (Double, Double), phi: (Double, Double), I: (Double, Double), zeta: (Double, Double)){
            def volume(): Double = pcnt * (4.0/3)*math.Pi*math.pow(pRad,3)
            def mass(): Double = pcnt * pMass
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