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
            val (xb, yb) = (4,4)
            val numP = 130
            val distSz = 600.0
            val zSz = 20.0
            val xs = Array.fill(numP)(scala.util.Random.nextDouble()*distSz)
            //val ys = xs.map(x => (2*scala.util.Random.nextInt(2)-1)*0.5*distSz*math.sin(4*math.Pi*x/distSz)+(distSz/20)*(scala.util.Random.nextDouble()-0.5))
            val ys = xs.map(x => scala.util.Random.nextInt(distSz.toInt/60+1)*60)
            val zs = xs.map(x => scala.util.Random.nextDouble()*zSz-zSz/2)
            val ps = (0 until numP by 1).map(i => GCCoord(xs(i),ys(i),0,0,zs(i),0,8.4))
            val cells = getCellStats(ps,xb,yb)
            cells.foreach(l => l.foreach(c => println(c.pcnt)))
            val probSet = makeProbSet(cells, xb,yb)
            println(" ")
            probSet.foreach(println(_))

            var systemVolume = ps.length * (4.0/3)*math.Pi*math.pow(ps(0).rad,3)
            println("systemVolume",systemVolume)

            val pDiv = 2
            val newPRad = ps(0).rad/pDiv
            val first = cells(0)(0)
            val last = cells(xb-1)(yb-1)
            val bounds = Seq(first.x0, first.y0, last.x0+last.xSz, last.y0+last.ySz)
            val newGrid = Array.fill(xb,yb)(mutable.ArrayBuffer[GCCoord]())

            while(systemVolume > 0){
                println("systemVolume",systemVolume)
                var added = 0.0
                while(added == 0.0){
                    val (i, j) = chooseCell(probSet, xb,yb)
                    val neighbors = Array.fill(9)(mutable.ArrayBuffer[GCCoord]())
                    for(ii <- i-1 to i+1){
                        for(jj <- j-1 to j+1) {
                            if(ii >= 0 && jj >= 0 && ii < xb && jj < yb){
                                println("ii",ii,"jj",jj,"i",i,"j",j)
                                neighbors(3*(ii-i+1) + (jj-j+1)) = newGrid(ii)(jj)
                            }
                        }
                    }
                    added = placeRandomParticle(cells(i)(j), neighbors.toSeq, pDiv)
                }
                systemVolume -= added
            }
            displayOldParticles(ps,bounds)
            displayNewParticles(newGrid,xb,yb,newPRad,bounds)

            val finalParticles = mutable.ArrayBuffer[GCCoord]()
            for(i <- 0 until xb){
                for(j <- 0 until yb){
                    for(p <- newGrid(i)(j)){
                        finalParticles += p
                    }
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

            //read in the particles as GCCoords
            val particles = CartAndRad.read(new File(dir, "CartAndRad."+step+".bin")).map(p => GCCoord(p))
            println(particles(0).getClass)
            val rho = 0.5 //g/cm^3
            val pVol = rho*(4.0/3)*math.Pi*math.pow(particles(0).rad,3)
            val numP = particles.length
            // //modify the particles in some way
            val (xBins,yBins) = (math.sqrt(numP/avgCnt).toInt, math.sqrt(numP/avgCnt).toInt)
            println(xBins,yBins)
            val cells = getCellStats(particles,xBins,yBins)
            val probSet = makeProbSet(cells, xBins, yBins)

            var systemVolume = particles.length * pVol
            println("systemVolume",systemVolume)

            val newPRad = particles(0).rad/div
            val first = cells(0)(0)
            val last = cells(xBins-1)(yBins-1)
            val bounds = Seq(first.x0, first.y0, last.x0+last.xSz, last.y0+last.ySz)
            val newGrid = Array.fill(xBins,yBins)(mutable.ArrayBuffer[GCCoord]())

            var reps = 0
            while(systemVolume > 0){
                if(reps == 1000){
                    reps = 0
                    println("systemVolume",systemVolume)
                }  
                var added = 0.0
                while(added == 0.0){
                    val (i, j) = chooseCell(probSet, xBins,yBins)
                    val neighbors = Array.fill(9)(mutable.ArrayBuffer[GCCoord]())
                    for(ii <- i-1 to i+1){
                        for(jj <- j-1 to j+1) {
                            if(ii >= 0 && jj >= 0 && ii < xBins && jj < yBins){
                                //println("ii",ii,"jj",jj,"i",i,"j",j)
                                neighbors(3*(ii-i+1) + (jj-j+1)) = newGrid(ii)(jj)
                            }
                        }
                    }
                    added = placeRandomParticle(cells(i)(j), neighbors.toSeq, div)
                }
                systemVolume -= added
                reps += 1
            }
            //displayOldParticles(particles,bounds)
            //displayNewParticles(newGrid,xBins,yBins,newPRad,bounds)

            val finalParticles = mutable.ArrayBuffer[Particle]()
            for(i <- 0 until xBins){
                for(j <- 0 until yBins){
                    for(p <- newGrid(i)(j)){
                        finalParticles += p.toCart
                    }
                }
            }
            //return the modified particles. Need to define newParticles!
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
                val pRad = if (cellParticles.length != 0) cellParticles(0).rad else 0.0
                //val allX = cellParticles.map(p => p.X)
                val X = meanAndStdev(cellParticles.map(p => p.X))
                //val allY = cellParticles.map(p => p.Y)
                val Y = meanAndStdev(cellParticles.map(p => p.Y))
                val e = meanAndStdev(cellParticles.map(p => p.e))
                val phi = meanAndStdev(cellParticles.map(p => p.phi))
                val I = meanAndStdev(cellParticles.map(p => p.i))
                val zeta = meanAndStdev(cellParticles.map(p => p.zeta))
                grid(i)(j) = Cell(x0,y0,xSpacing,ySpacing,cellParticles.length,pRad,X,Y,e,phi,I,zeta)
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
    def placeRandomParticle(cell: Cell, newNearCells: Seq[mutable.ArrayBuffer[GCCoord]], pDiv: Double): Double = {
        val maxAttempts = 100
        var newP = GCCoord(0,0,0,0,0,0,-1)
        var attempts = 0
        while(attempts < maxAttempts && (checkOverlap(newP,newNearCells) || newP.rad < 0)){
            val X = scala.util.Random.nextDouble()*cell.xSz + cell.x0
            val Y = scala.util.Random.nextDouble()*cell.ySz + cell.y0
            val e = scala.util.Random.nextGaussian()*cell.e._2 + cell.e._1
            val phi = scala.util.Random.nextGaussian()*cell.phi._2 + cell.phi._1
            val I = scala.util.Random.nextGaussian()*cell.I._2 + cell.I._1
            val zeta = scala.util.Random.nextGaussian()*cell.zeta._2 + cell.zeta._1
            val pRad = cell.pRad/pDiv
            newP = GCCoord(X,Y,e,phi,I,zeta,pRad)
            //println("Trying to put new particle at ",X,Y)
            attempts += 1
            //println("attempts",attempts)
        }
        if(attempts < maxAttempts){
            //println("Successful placement at ",newP.X,newP.Y)   
            newNearCells(4) += newP
            return (4.0/3)*math.Pi*newP.rad*newP.rad*newP.rad//math.pow(newP.rad,3)
        }
        else {
            println("Couldn't place the particle in "+maxAttempts+" attempts")
            return 0.0
        }
    }

    def checkOverlap(newParticle: GCCoord, newNearCells: Seq[mutable.ArrayBuffer[GCCoord]]): Boolean = {
        val currParticles = newNearCells(4) //center cell
        var overlaps = false
        for(p <- currParticles){
            if(particleOverlap(newParticle, p)){
                //println("overlaps with particle at",p.X,p.Y)
                //println("already",currParticles.length,"particles in the cell")
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
        val distSqr = (p1.toCart).distSqr(p2.toCart)
        return (distSqr < (p1.rad + p2.rad)*(p1.rad + p2.rad))
    }

    def meanAndStdev(set: Seq[Double]): (Double, Double) = {
        val mean = set.sum/set.length
        val stdev = math.sqrt(set.map(x => math.pow(x-mean,2)).sum/set.length)
        (mean, stdev)
    }

    def displayNewParticles(newGrid: Array[Array[mutable.ArrayBuffer[GCCoord]]], xb:Int, yb:Int, pRad:Double, bounds: Seq[Double]): Unit = {
        val pxSize = 1000
        val xSize = bounds(2)-bounds(0)
        val ySize = bounds(3)-bounds(1)
        val allX = mutable.ArrayBuffer[Double]()
        val allY = mutable.ArrayBuffer[Double]()
        for(i <- 0 until xb){
            for(j <- 0 until yb){
                for(p <- newGrid(i)(j).map(_.toCart)){
                    println("Adding (x,y) = ",p.x,p.y,"to the plot")
                    allX += p.x
                    allY += p.y
                }
            }
        }
        val style = ScatterStyle(allX.toSeq, allY.toSeq, symbolWidth=pRad*(1000/xSize), symbolHeight=pRad*(1000/ySize))
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
        val style = ScatterStyle(allX.toSeq, allY.toSeq, symbolWidth=pRad*(1000/xSize), symbolHeight=pRad*(1000/ySize))
        val plot = Plot.simple(style, title="Old Particles")
            .updatedAxis[NumericAxis]("x", _.min(bounds(0)).max(bounds(2)).updatedName("x").numberFormat("%1.2f"))
            .updatedAxis[NumericAxis]("y", _.min(bounds(1)).max(bounds(3)).updatedName("y").numberFormat("%1.2f"))
        val updater = if (true) Some(SwingRenderer(plot, pxSize, pxSize, true)) else None
    }
    
    case class Cell(x0: Double, y0: Double, xSz:Double, ySz: Double, pcnt: Int, pRad: Double, X: (Double, Double), 
        Y: (Double, Double), e: (Double, Double), phi: (Double, Double), I: (Double, Double), zeta: (Double, Double)){
            def volume(): Double = pcnt * (4.0/3)*math.Pi*math.pow(pRad,3)
    }
}