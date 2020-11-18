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
            val (xb, yb) = (2,2)
            val xs = Seq(0, 3, 6, 1, 2, 10)
            val ys = Seq(0, 1, 2, 6, 8, 10)
            val ps = (0 to 5 by 1).map(i => GCCoord(xs(i),ys(i),0,0,0,0,0.5))
            val cells = getCellStats(ps,xb,yb,1)
            cells.foreach(l => l.foreach(c => println(c.pcnt)))
            val probSet = makeProbSet(cells, xb,yb)
            println(" ")
            probSet.foreach(println(_))
            for(k <- 0 until 10){
                val (i, j) = chooseCell(probSet, xb,yb)
                println("i, j",i,j)
            }

            var systemVolume = ps.length * (4.0/3)*math.Pi*math.pow(ps(0).rad,3)
            val pDiv = 2
            val newGrid = Array.fill(xb,yb)(mutable.ArrayBuffer[GCCoord]())
            while(systemVolume > 0){
                println("systemVolume",systemVolume)
                val (i, j) = chooseCell(probSet, xb,yb)
                val neighbors = Array.fill(9)(mutable.ArrayBuffer[GCCoord]())
                for(ii <- i-1 to i+1){
                    for(jj <- j-i to j+1) {
                        if(ii >= 0 && jj >= 0 && ii < xb && jj < yb){
                            neighbors(3*(ii-i+1) + (jj-j+1)) = newGrid(ii)(jj)
                        }
                    }
                }
                systemVolume -= placeRandomParticle(cells(i)(j), neighbors.toSeq, pDiv)
            }
            println("systemVolume",systemVolume)

            //finalParticles = newGrid.map()

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
                //val allX = cellParticles.map(p => p.X)
                val X = meanAndStdev(cellParticles.map(p => p.X))
                //val allY = cellParticles.map(p => p.Y)
                val Y = meanAndStdev(cellParticles.map(p => p.Y))
                val e = meanAndStdev(cellParticles.map(p => p.e))
                val phi = meanAndStdev(cellParticles.map(p => p.phi))
                val I = meanAndStdev(cellParticles.map(p => p.i))
                val zeta = meanAndStdev(cellParticles.map(p => p.zeta))
                grid(i)(j) = Cell(x0,y0,xSpacing,ySpacing,cellParticles.length,pRad,pMass,X,Y,e,phi,I,zeta)
            }
        }
        grid
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
        println("rand",rand,"index",index)
        (index/yBins, index%yBins)
    }

    def binarySearch(seq: Seq[Double], v: Double): Int = {
        var cnt = 0
        var (l, r) = (0, seq.length-1)
        while(l < r){
            println("v",v,"l,r",l,r)
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
    def placeRandomParticle(cell: Cell, newNearCells: Seq[mutable.ArrayBuffer[GCCoord]], pDiv: Double): Double = {
        var newP = GCCoord(0,0,0,0,0,0,-1)
        
        while(checkOverlap(newP,newNearCells)){
            val X = scala.util.Random.nextDouble()*cell.xSz + cell.x0
            val Y = scala.util.Random.nextDouble()*cell.ySz + cell.y0
            val e = scala.util.Random.nextGaussian()*cell.e._2 + cell.e._1
            val phi = scala.util.Random.nextGaussian()*cell.phi._2 + cell.phi._1
            val I = scala.util.Random.nextGaussian()*cell.I._2 + cell.I._1
            val zeta = scala.util.Random.nextGaussian()*cell.zeta._2 + cell.zeta._1
            val pRad = cell.pRad/pDiv
            newP = GCCoord(X,Y,e,phi,I,zeta,pRad)
            println("Trying to put new particle at ",X,Y)
        }
        println("Successful placement at ",newP.X,newP.Y)   
        newNearCells(4) += newP
        return (4.0/3)*math.Pi*math.pow(newP.rad,3)
    }

    def checkOverlap(newParticle: GCCoord, newNearCells: Seq[mutable.ArrayBuffer[GCCoord]]): Boolean = {
        val currParticles = newNearCells(4) //center cell
        for(p <- currParticles){
            if(particleOverlap(newParticle, p)){
                return true
            }
        }
        for(i <- 0 until 3){
            for(j <- 0 until 3){
                if(!(i==1 && j==1)){
                    for(p <- newNearCells(i*3 + j)){
                        if(particleOverlap(newParticle, p)){
                            return true
                        }
                    }
                }
            }
        }
        return false
    }

    def particleOverlap(p1: GCCoord, p2: GCCoord): Boolean = {
        if (p1.rad < 0 || p2.rad < 0) return true
        val dist = (p1.toCart).distance(p2.toCart)
        return (dist < (p1.rad + p2.rad))
    }

    def meanAndStdev(set: Seq[Double]): (Double, Double) = {
        val mean = set.sum/set.length
        val stdev = math.sqrt(set.map(x => math.pow(x-mean,2)).sum/set.length)
        (mean, stdev)
    }
    
    case class Cell(x0: Double, y0: Double, xSz:Double, ySz: Double, pcnt: Int, pRad: Double, pMass: Double, X: (Double, Double), 
        Y: (Double, Double), e: (Double, Double), phi: (Double, Double), I: (Double, Double), zeta: (Double, Double)){
            def volume(): Double = pcnt * (4.0/3)*math.Pi*math.pow(pRad,3)
        }
}