package simprocessing

import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer

import data.CartAndRad
import util.Particle
import util.GCCoord

import java.io.File
import scala.math
import scala.collection.mutable

object PreparePWPSMod {
	def main(args: Array[String]): Unit = {
        val TESTING = false
        if(TESTING){
            val (xBins, yBins) = (2,2)
            val numP = 5
            val div = 2
            val distSz = 600.0
            val zSz = 20.0
            val xs = Array.fill(numP)(scala.util.Random.nextDouble()*distSz)
            val ys = xs.map(x => (2*scala.util.Random.nextInt(2)-1)*0.5*distSz*math.sin(4*math.Pi*x/distSz)+(distSz/20)*(scala.util.Random.nextDouble()-0.5))
            val zs = xs.map(x => scala.util.Random.nextDouble()*zSz-zSz/2)
            //val xs = Seq(0,1,3,4)
            //val ys = Seq(4,1,3,0)
            //val zs = Seq(0,0,0,0)
            val cartParticles = (0 until numP by 1).map(i => Particle(xs(i),ys(i),zs(i),0,0,0,8.4))
            val particles = cartParticles.map(p => GCCoord(p))
            val pVol = (4.0/3)*math.Pi*math.pow(particles(0).rad,3)
            // //modify the particles in some way
            println(xBins,yBins)
            val cells = getCellStats(particles,xBins,yBins)
            val probSet = makeProbSet(cells, xBins, yBins)

            var systemVolume = particles.length * pVol
            println("systemVolume",systemVolume)
            val newPRad = particles(0).rad/div
        
            val newGrid = Array.ofDim[NewCell](xBins,yBins)
            val xMin = cartParticles.map(p => p.x).min
            val xMax = cartParticles.map(p => p.x).max
            val xStep = (xMax-xMin)/xBins
            val yMin = cartParticles.map(p => p.y).min
            val yMax = cartParticles.map(p => p.y).max
            val yStep = (yMax-yMin)/yBins
            for(i <- 0 until xBins){
                for(j <- 0 until yBins){
                    val xmin = if(i == 0) Double.MinValue else if(i == xBins-1) Double.MaxValue else i*xStep + xMin
                    val xmax = if(i == 0) xStep else if(i == xBins-1) Double.MaxValue else xmin + xStep
                    val ymin = if(j == 0) Double.MinValue else if(j == yBins-1) Double.MaxValue else j*yStep + yMin
                    val ymax = if(j == 0) yStep else if(j == yBins-1) Double.MaxValue else ymin + yStep
                    newGrid(i)(j) = NewCell(xmin,ymin,xmax,ymax,mutable.ArrayBuffer[Particle]()) 
                }
            }

            var reps = 0
            while(systemVolume > 0){
                if(reps == 1000){
                    reps = 0
                    println("systemVolume",systemVolume)
                }  
                var added = 0.0
                while(added == 0.0){
                    val (i, j) = chooseCell(probSet, xBins,yBins)
                    added = placeRandomParticle(cells(i)(j), newGrid, xStep, yStep, xMin, yMin, div)
                }
                systemVolume -= added
                reps += 1
            }
            val bounds = Seq(xMin, yMin, xMax, yMax)
            displayOldParticles(particles,bounds)
            displayNewParticles(newGrid,xBins,yBins,newPRad,bounds)

            val finalParticles = mutable.ArrayBuffer[Particle]()
            for(i <- 0 until xBins){
                for(j <- 0 until yBins){
                    for(p <- newGrid(i)(j).particles){
                        finalParticles += p
                    }
                }
            }
            for(p <- finalParticles){
                for(pp <- finalParticles){
                        if(p != pp && p.distance(pp) < p.rad+pp.rad) println("Overlap in output")
                }
            }
        }

        else{ //Not Testing!
            if (args.contains("-help") || args.length < 1) {
                println("Arguments:")
                println("\t-dir path: path to directory, defaults to current directory")
                println("\t-step #: the step you want to base the new file off of. Defaults to 0")
                println("\t-div #: the ratio of the current particle radius to the new particle radius. Defaults to 2")
                println("\t-cnt #: the avg. number of particles per bin. Defaults to 100")
                println("\t-width #: width of window/image in pixels, defaults to 1000")
                println("\t-height #: height of window/image in pixels, defaults to 1000")
                println("\t-display: tells if the image should be displayed in a window")
                println("\t-save prefix: tells if images should be saved and gives prefix")
                println("\t-azMin #:the minimum azimuthal value to display in the surface plot")
                println("\t-azMax #:the maximum azimuthal value to display in the surface plot")
                sys.exit()
            }
            val dir = new File(args.sliding(2).find(_(0) == "-dir").map(_(1)).getOrElse("."))
            val step = args.sliding(2).find(_(0) == "-step").map(_(1)).getOrElse("0")
            val div = args.sliding(2).find(_(0) == "-div").map(_(1).toInt).getOrElse(2)
            val avgCnt = args.sliding(2).find(_(0) == "-cnt").map(_(1).toInt).getOrElse(100)
            val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
            val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
            val display = args.contains("-display")
            val save = args.sliding(2).find(_(0) == "-save").map(_(1))
            val azMin = args.sliding(2).find(_(0) == "-azMin").map(_(1).toDouble).getOrElse(Double.MinValue)
            val azMax = args.sliding(2).find(_(0) == "-azMax").map(_(1).toDouble).getOrElse(Double.MaxValue)

            //read in the particles, switch to GCCoords
            val cartParticles = CartAndRad.read(new File(dir, "CartAndRad."+step+".bin"))
            val particles = cartParticles.map(p => GCCoord(p))
            println(particles(0).getClass)
            //val rho = 0.5 //g/cm^3
            val pVol = (4.0/3)*math.Pi*math.pow(particles(0).rad,3)
            val numP = particles.length
            // //modify the particles in some way
            val (xBins,yBins) = (math.sqrt(numP/avgCnt).toInt, math.sqrt(numP/avgCnt).toInt)
            println(xBins,yBins)
            val cells = getCellStats(particles,xBins,yBins)
            val probSet = makeProbSet(cells, xBins, yBins)

            var systemVolume = particles.length * pVol
            println("systemVolume",systemVolume)

            val newGrid = Array.ofDim[NewCell](xBins,yBins)
            val xMin = cartParticles.map(p => p.x).min
            val xMax = cartParticles.map(p => p.x).max
            val xStep = (xMax-xMin)/xBins
            val yMin = cartParticles.map(p => p.y).min
            val yMax = cartParticles.map(p => p.y).max
            val yStep = (yMax-yMin)/yBins
            for(i <- 0 until xBins){
                for(j <- 0 until yBins){
                    val xmin = if(i == 0) Double.MinValue else if(i == xBins-1) Double.MaxValue else i*xStep + xMin
                    val xmax = if(i == 0) xStep else if(i == xBins-1) Double.MaxValue else xmin + xStep
                    val ymin = if(j == 0) Double.MinValue else if(j == yBins-1) Double.MaxValue else j*yStep + yMin
                    val ymax = if(j == 0) yStep else if(j == yBins-1) Double.MaxValue else ymin + yStep
                    newGrid(i)(j) = NewCell(xmin,ymin,xmax,ymax,mutable.ArrayBuffer[Particle]()) 
                }
            }

            var reps = 0
            val newPRad = particles(0).rad/div
            val newPVol = (4.0/3)*math.Pi*newPRad*newPRad*newPRad
            while(systemVolume-newPVol > 0.0){
                if(reps == 1000){
                    reps = 0
                    println("systemVolume",systemVolume)
                }  
                var added = 0.0
                while(added == 0.0){
                    val (i, j) = chooseCell(probSet, xBins,yBins)
                    added = placeRandomParticle(cells(i)(j), newGrid, xStep, yStep, xMin, yMin, div)
                }
                systemVolume -= added
                reps += 1
            }
            //displayOldParticles(particles,bounds)
            //displayNewParticles(newGrid,xBins,yBins,newPRad,bounds)

            val finalParticles = mutable.ArrayBuffer[Particle]()
            for(i <- 0 until xBins){
                for(j <- 0 until yBins){
                    for(p <- newGrid(i)(j).particles){
                        finalParticles += p
                    }
                }
            }
            //return the modified particles
            CartAndRad.write(new File(dir, "CartAndRadMod."+step+".bin"), finalParticles)
        }
    }

    def getCellStats(gcParticles: Seq[GCCoord], xBins: Int, yBins: Int): Array[Array[Cell]] = {
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
                val pRad = if (cellParticles.length != 0) cellParticles(0).rad else -1.0
                //val allX = cellParticles.map(p => p.X)
                val X = meanAndStdev(cellParticles.map(p => p.X))
                //val allY = cellParticles.map(p => p.Y)
                val Y = meanAndStdev(cellParticles.map(p => p.Y))
                val e = meanAndStdev(cellParticles.map(p => p.e))
                val phi = meanAndStdev(mapAngles(cellParticles.map(p => p.phi)))
                val I = meanAndStdev(cellParticles.map(p => p.i))
                val zeta = meanAndStdev(mapAngles(cellParticles.map(p => p.zeta)))
                grid(i)(j) = Cell(x0,y0,xSpacing,ySpacing,cellParticles.length,pRad,X,Y,e,phi,I,zeta)
            }
        }
        grid
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

    def makeProbSet(cells: Array[Array[Cell]], xBins: Int, yBins: Int): Seq[Double] = {
        val ret = Array.ofDim[Double](xBins*yBins)
        var sum = 0
        for(i <- 0 until xBins){
            for(j <- 0 until yBins){
                val curr = cells(i)(j).pcnt
                sum += curr
                ret(i*yBins + j) = sum
            }
        }
        ret.map(x => x/sum).toSeq
    }

    def chooseCell(probSet: Seq[Double], xBins: Int, yBins: Int): (Int, Int) = {
        val rand = scala.util.Random.nextDouble()
        val index = binarySearch(probSet, rand)
        //println("rand",rand,"index",index)
        (index/yBins, index%yBins)
    }

    def binarySearch(seq: Seq[Double], v: Double): Int = {
        var cnt = 0
        var (l, r) = (0, seq.length-1)
        while(l < r){
            //println("v",v,"l,r",l,r)
            val midpt = (r-l)/2 + l
            val (lower, upper) = if(midpt != 0) (seq(midpt-1), seq(midpt)) else (0.0, seq(midpt))
            if(lower < v && upper >= v){
                    return(midpt)
            }
            else if(v <= lower){
                    r = midpt-1
            }
            else {
                    l = midpt+1
            }
        //cnt += 1
        //if(cnt > 1000) sys.exit()
        }
        return(if(v <= seq(l)) l else l+1)
    }

    //return the volume added to the new distribution
    def placeRandomParticle(cell: Cell, newGrid: Array[Array[NewCell]], dx: Double, dy:Double, xMin:Double, yMin:Double, pDiv: Double): Double = {
        val maxAttempts = 100
        var newP = Particle(0,0,0,0,0,0,-1)
        var (ii, jj) = (0,0)
        var attempts = 0
        while(attempts < maxAttempts && (newP.rad < 0 || checkOverlap(newP,ii,jj,newGrid))){
            val X = scala.util.Random.nextDouble()*cell.xSz + cell.x0
            val Y = scala.util.Random.nextDouble()*cell.ySz + cell.y0
            val e = scala.util.Random.nextGaussian()*cell.e._2 + cell.e._1
            val phi = scala.util.Random.nextGaussian()*cell.phi._2 + cell.phi._1
            val I = scala.util.Random.nextGaussian()*cell.I._2 + cell.I._1
            val zeta = scala.util.Random.nextGaussian()*cell.zeta._2 + cell.zeta._1
            val pRad = cell.pRad/pDiv
            newP = GCCoord(X,Y,e,phi,I,zeta,pRad).toCart
            ii = math.round(((newP.x-xMin) - (newP.x-xMin) % dx)/dx).toInt
            if(ii >= newGrid.length){ 
                //println(ii - newGrid.length+1)
                ii = newGrid.length-1
            }
            else if(ii < 0){
                //println(ii)
                ii = 0
            } 
            jj = math.round(((newP.y-yMin) - (newP.y-yMin) % dy)/dy).toInt
            if(jj >= newGrid(0).length) {
                //println(jj - newGrid(0).length+1)
                jj = newGrid(0).length-1
            }
            else if(jj < 0) {
                //println(jj)
                jj = 0
            }
            attempts += 1
            //println("attempts",attempts)
        }
        if(attempts < maxAttempts){
            //println("Successful placement at ",newP.X,newP.Y)   
            newGrid(ii)(jj).particles += newP
            return (4.0/3)*math.Pi*newP.rad*newP.rad*newP.rad//math.pow(newP.rad,3)
        }
        else {
            println("Couldn't place the particle in "+maxAttempts+" attempts")
            return 0.0
        }
    }

    def checkOverlap(newParticle: Particle, ii:Int, jj:Int, newGrid: Array[Array[NewCell]]): Boolean = {
        val xb = newGrid.length
        val yb = newGrid(0).length
        val currNewCell = newGrid(ii)(jj)
        var overlaps = false
        for(p <- currNewCell.particles){
            if(particleOverlap(newParticle, p)){
                //println("overlaps with particle at",p.X,p.Y)
                //println("already",currParticles.length,"particles in the cell")
                return true
            }
        }
        val shouldCheck = Array(0,0,0,0,0,0,0,0,0)
        if(currNewCell.xmin + 2*newParticle.rad > newParticle.x){
            shouldCheck(0) = 1
            shouldCheck(1) = 1
            shouldCheck(2) = 1
        }
        if(currNewCell.xmax - 2*newParticle.rad < newParticle.x){
            shouldCheck(6) = 1
            shouldCheck(7) = 1
            shouldCheck(8) = 1
        }
        if(currNewCell.ymin + 2*newParticle.rad > newParticle.y){
            shouldCheck(2) = 1
            shouldCheck(5) = 1
            shouldCheck(8) = 1
        }
        if(currNewCell.ymax - 2*newParticle.rad < newParticle.y){
            shouldCheck(0) = 1
            shouldCheck(3) = 1
            shouldCheck(6) = 1
        }
        for(i <- 0 until 3){
            for(j <- 0 until 3){
                val k = i-1 + ii
                val l = j-1 + jj
                if(shouldCheck(i*3 + j) == 1 && k >= 0 && k < xb && l >= 0 && l < yb){
                    for(p <- newGrid(k)(l).particles){
                        if(particleOverlap(newParticle, p)){
                            return true
                        }
                    }
                }
            }
        }
        return false
    }

    def particleOverlap(p1: Particle, p2: Particle): Boolean = {
        if (p1.rad < 0 || p2.rad < 0) return true
        val distSqr = p1.distSqr(p2)
        return (distSqr < (p1.rad + p2.rad)*(p1.rad + p2.rad))
    }

    def meanAndStdev(set: Seq[Double]): (Double, Double) = {
        val mean = set.sum/set.length
        val stdev = math.sqrt(set.map(x => math.pow(x-mean,2)).sum/set.length)
        (mean, stdev)
    }

    def displayNewParticles(newGrid: Array[Array[NewCell]], xb:Int, yb:Int, pRad:Double, bounds: Seq[Double]): Unit = {
        val pxSize = 1000
        val xSize = bounds(2)-bounds(0)
        val ySize = bounds(3)-bounds(1)
        val allX = mutable.ArrayBuffer[Double]()
        val allY = mutable.ArrayBuffer[Double]()
        for(i <- 0 until xb){
            for(j <- 0 until yb){
                for(p <- newGrid(i)(j).particles){
                    println("Adding (x,y) = ",p.x,p.y,"to the plot")
                    allX += p.x
                    allY += p.y
                }
            }
        }
        val style = ScatterStyle(allX.toSeq, allY.toSeq, symbolWidth=2*pRad*(1000/xSize), symbolHeight=2*pRad*(1000/ySize))
        val plot = Plot.simple(style, title="New Particles")
            .updatedAxis[NumericAxis]("x", _.min(bounds(0)).max(bounds(2)).updatedName("x").numberFormat("%1.2f"))
            .updatedAxis[NumericAxis]("y", _.min(bounds(1)).max(bounds(3)).updatedName("y").numberFormat("%1.2f"))
        val updater = if (true) Some(SwingRenderer(plot, pxSize, pxSize, true)) else None
    }

    def displayOldParticles(oldParticles: Seq[GCCoord], bounds: Seq[Double]): Unit = {
        val pxSize = 1000
        val pRad = oldParticles(0).rad
        val xSize = bounds(2)-bounds(0)
        val ySize = bounds(3)-bounds(1)
        val allX = oldParticles.map(p => p.toCart.x)
        val allY = oldParticles.map(p => p.toCart.y)
        val style = ScatterStyle(allX.toSeq, allY.toSeq, symbolWidth=2*pRad*(1000/xSize), symbolHeight=2*pRad*(1000/ySize))
        val plot = Plot.simple(style, title="Old Particles")
            .updatedAxis[NumericAxis]("x", _.min(bounds(0)).max(bounds(2)).updatedName("x").numberFormat("%1.2f"))
            .updatedAxis[NumericAxis]("y", _.min(bounds(1)).max(bounds(3)).updatedName("y").numberFormat("%1.2f"))
        val updater = if (true) Some(SwingRenderer(plot, pxSize, pxSize, true)) else None
    }
    
    case class Cell(x0: Double, y0: Double, xSz:Double, ySz: Double, pcnt: Int, pRad: Double, X: (Double, Double), 
        Y: (Double, Double), e: (Double, Double), phi: (Double, Double), I: (Double, Double), zeta: (Double, Double)){
            def volume(): Double = pcnt * (4.0/3)*math.Pi*math.pow(pRad,3)
    }
    case class NewCell(xmin: Double, ymin: Double, xmax:Double, ymax: Double, particles: mutable.ArrayBuffer[Particle]){
        
    }
}