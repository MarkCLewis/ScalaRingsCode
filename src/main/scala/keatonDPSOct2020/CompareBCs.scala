package keatonDPSOct2020

import data.CartAndRad
import data.FixedBinned
import util.Particle

import java.io.File
import scala.collection.mutable
import scala.math

import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer

object CompareBCs {
    val MOON_ORBIT_RADIUS = 139380
    val PARTICLE_RADIUS = 0.00000006 //6e-8
    val CELL_WIDTH = 4*15.0/MOON_ORBIT_RADIUS

	def main(args: Array[String]): Unit = {
        if (args.contains("-help") || args.length < 1) {
            println("Arguments:")
            println("\t-pwpsdir path: path to PWPS directory, defaults to current directory")
            println("\t-soadir path: path to SOA directory, defaults to current directory")
            println("\t-globdir path: path to Global directory, defaults to current directory")
            println("\t-bin soaBinsX soaBinsY globBinsX globBinsY: set the number of bins on each axis (if unfilled defaults to a scatterplot of all particles")
            println("\t-width #: width of global window/image in pixels, defaults to 1000")
            println("\t-height #: height of global window/image in pixels, defaults to 1000")
            println("\t-display: tells if the image should be displayed in a window")
            println("\t-save prefix: tells if images should be saved and gives prefix")
            //println("\t-stepRange #: the (inclusive) range of step numbers you want to look at i.e. CartAndRad.xxxxx.bin - CartAndRad.yyyyy.bin. Defaults to step zero")
            sys.exit()
        }
        val pwpsdir = new File(args.sliding(2).find(_(0) == "-pwpsdir").map(_(1)).getOrElse("."))
        val soadir = new File(args.sliding(2).find(_(0) == "-soadir").map(_(1)).getOrElse("."))
        val globdir = new File(args.sliding(2).find(_(0) == "-globdir").map(_(1)).getOrElse("."))
        val (soaBinsX, soaBinsY, globBinsX, globBinsY) = args.sliding(5).find(_(0) == "-bin").map(arg => (arg(1).toInt, arg(2).toInt, arg(3).toInt, arg(4).toInt)).getOrElse(100,50,100,60)
        val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
        val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
        val display = args.contains("-display")
        val save = args.sliding(2).find(_(0) == "-save").map(_(1))        

        //Check how far the simulation is azimuthally. Can extract the data for binning
        //SOA and global start at same place, bottom of cell hits the moon after about 100 steps, PWPS hits moon after 3
        val lastSoaStep = soadir.listFiles.filter(file => file.isFile && file.getName.contains("CartAndRad"))
             .map(file => file.getName.stripPrefix("CartAndRad.").stripSuffix(".bin").trim.toInt).max -1100
        val lastGlobStep = Seq(globdir.listFiles.filter(file => file.isFile && file.getName.contains("CartAndRad"))
            .map(file => file.getName.stripPrefix("CartAndRad.").stripSuffix(".bin").trim.toInt).max, lastSoaStep).min
        //val soaStep = lastGlobStep + 100
        //val globParticles = CartAndRad.read(new File(globdir, "CartAndRad."+lastGlobStep+".bin"))
        println("last step in global: ", lastGlobStep)
        val doGlobal = true
        val gradCut = 1.0/5

    
        val (start, end) = (lastGlobStep, lastGlobStep)
        for(step <- start to end by 100){
            //First make soa plot
            // 

            //need best way to do this with combining the 3 steps together
            val soaParticles = CartAndRad.read(new File(soadir, "CartAndRad."+(step+100)+".bin"))
            val soaMinY = soaParticles.map(_.y).min
            val soaMaxY = soaParticles.map(_.y).max
            println("SOA min azimuth: ", soaMinY)
            val soaParticlesPrev = CartAndRad.read(new File(soadir, "CartAndRad."+(step-900)+".bin"))
            //val soaPrevMinY = soaParticlesPrev.map(_.y).min
            val soaParticlesNext = CartAndRad.read(new File(soadir, "CartAndRad."+(step+1100)+".bin"))
                .filter(p => p.y < soaMinY)
            val soaParticles3x = soaParticlesPrev++soaParticles++soaParticlesNext

            val (soaBinnedX, soaBinnedY, soaTau) = getTau(soaParticles3x,soaBinsX,soaBinsY)
            val soaModY = soaBinnedY.map(y => (-y % (2*math.Pi)))
            val soaModX = soaBinnedX.map(x => -x)
            val soaAvgTau = getRealAvg(soaTau, gradCut)
            val gradientSOA = ColorGradient(0.0 -> BlackARGB, soaAvgTau -> BlueARGB, soaTau.max -> GreenARGB)
            val soaSurfGrp = SeqToIntSeries((0 until (soaBinsX*soaBinsY) by 1).toSeq.map(i => i/soaBinsY))
            val (soaSliceBinX, soaTauSlice) = getSurfDensitySlice(soaParticles,200,0.1)
            val soaSurfStyle = new ColoredSurfaceStyle(soaModX,soaModY,group=soaSurfGrp,colors=gradientSOA(soaTau))
            val soaSliceStyle = ScatterStyle(soaSliceBinX.map(x => -x), soaTauSlice.map(t => t/soaTauSlice.max), symbolWidth=5, symbolHeight=5)
            
            val soaSurf = Plot.simple(soaSurfStyle, title = ("SOA Surface Plot "+step))
                .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f"))))
            val updaterSOASurf = if (display) Some(SwingRenderer(soaSurf, width, height, true)) else None
            println("Done drawing SOA Surface Step # " + step)
            val soaSlice = Plot.scatterPlot(soaSliceBinX.map(x => -x),soaTauSlice,symbolColor=BlackARGB,symbolSize=5,title=("Slice SOA "+step),xLabel="radial",yLabel="coveredArea")
                .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
            //save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix+ "soaSlicePlot" + s".$step.png", width = width, height = height))
            val updaterSOASlice = if (display) Some(SwingRenderer(soaSlice, width, height, true)) else None
            println("Done drawing SOA Slice Step # " + step)


            //Now for the PWPS Plots
            //
            val (categories, allFixedBins) = FixedBinned.read(new File(pwpsdir, "FixedBinned.bin").getAbsolutePath())
            val sgn = math.signum(allFixedBins(1)(0)(0) - allFixedBins(0)(0)(0))
            val fixedBins = allFixedBins.filter(col => col(0)(0) <= -(2*soaMinY-soaMaxY) && col(0)(0) >= -(2*soaMaxY-soaMinY))
            println("FixedBins Length: " + fixedBins.length)
            if(true) categories.zipWithIndex.foreach(println)
            val len = fixedBins.length * fixedBins(1).length
            val sliceIndex = fixedBins.length/2
            println("len", len)
            val allFBX = Array.ofDim[Double](len)
            val allFBY = Array.ofDim[Double](len)//mutable.ArrayBuffer.empty[Double]
            val allFBTau = Array.ofDim[Double](len)//mutable.ArrayBuffer.empty[Double]
            val pwpsSliceX = Array.ofDim[Double](fixedBins(0).length)
            val pwpsSliceTau = Array.ofDim[Double](fixedBins(0).length)
            for(i <- 0 until fixedBins(0).length){ //number of columns per step
                for(j <- 0 until fixedBins.length){ //number of steps
                    allFBX(i*fixedBins.length + j) = fixedBins(j)(i)(1)
                    allFBY(i*fixedBins.length + j) = fixedBins(j)(i)(0)
                    allFBTau(i*fixedBins.length + j) = fixedBins(j)(i)(4)
                    if(j == sliceIndex){
                        pwpsSliceX(i) = fixedBins(j)(i)(1)
                        pwpsSliceTau(i) = fixedBins(j)(i)(4)
                    }
                }
            }
            val pwpsModY = allFBY.map(y => y % (2*math.Pi))
            //val pwpsNonZeroTau = allFBTau.filter(_ > allFBTau.max/3)
            val pwpsAvgTau = getRealAvg(allFBTau, gradCut)//pwpsNonZeroTau.sum/pwpsNonZeroTau.length
            println("pwps avg",pwpsAvgTau)
            val gradientPWPS = ColorGradient(0.0 -> BlackARGB, pwpsAvgTau -> BlueARGB, allFBTau.max -> GreenARGB)
            val pwpsSurfGrp = SeqToIntSeries((0 until len by 1).toSeq.map(i => i/fixedBins.length))
            val pwpsSurfStyle = new ColoredSurfaceStyle(allFBX,pwpsModY,group=pwpsSurfGrp,colors=gradientPWPS(allFBTau))
            val pwpsSliceStyle = ScatterStyle(pwpsSliceX, pwpsSliceTau.map(t => t/pwpsSliceTau.max), symbolWidth=5, symbolHeight=5)
            
            val pwpsSurf = Plot.simple(pwpsSurfStyle, title = ("PWPS Surface Plot "+step))
                .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f"))))
            val updaterPWPSSurf = if (display) Some(SwingRenderer(pwpsSurf, width, height, true)) else None
            println("Done drawing PWPS Surface Step # " + step)
            val pwpsSlice = Plot.scatterPlot(pwpsSliceX,pwpsSliceTau,symbolColor=BlackARGB,symbolSize=5,title=("Slice PWPS "+step),xLabel="radial",yLabel="coveredArea")
                .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
            //save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix+ "soaSlicePlot" + s".$step.png", width = width, height = height))
            val updaterPWPSSlice = if (display) Some(SwingRenderer(pwpsSlice, width, height, true)) else None
            println("Done drawing PWPS Slice Step # " + step)

            
            if(doGlobal){
                //Now the globals
                //

                //use y = (y + 2 * math.Pi) % (2*math.Pi) for all 3
                val globParticles = CartAndRad.read(new File(globdir, "CartAndRad."+step+".bin"))
                    .filter(p => (p.y + 2*math.Pi) % (2*math.Pi) <= -(2*(soaMinY)-soaMaxY) % (2*math.Pi) && 
                    (p.y + 2*math.Pi) % (2*math.Pi) >= -(2*soaMaxY-soaMinY) % (2*math.Pi))
                val (globBinnedX, globBinnedY, globTau) = getTau(globParticles,globBinsX,globBinsY)
                val globModY = globBinnedY.map(y => (y + 2*math.Pi) % (2*math.Pi))
                val globTauAvg = getRealAvg(globTau, gradCut)
                val gradientGlobal = ColorGradient(0.0 -> BlackARGB, globTauAvg -> BlueARGB, globTau.max -> GreenARGB)
                val globSurfGrp = SeqToIntSeries((0 until (globBinsX*globBinsY) by 1).toSeq.map(i => i/globBinsY))
                val (globSliceBinX, globTauSlice) = getSurfDensitySlice(globParticles,50,0.1)
                val globSurfStyle = new ColoredSurfaceStyle(globBinnedX,globModY,group=globSurfGrp,colors=gradientGlobal(globTau))
                val globSliceStyle = ScatterStyle(globSliceBinX, globTauSlice.map(t => t/globTauSlice.max), symbolWidth=5, symbolHeight=5)
                
                val globSurf = Plot.simple(globSurfStyle, title = ("Global Surface Plot "+step))
                    .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                    .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f"))))
                val updaterGlobSurf = if (display) Some(SwingRenderer(globSurf, width, height, true)) else None
                println("Done drawing Global Surface Step # " + step)
                val globSlice = Plot.scatterPlot(globSliceBinX,globTauSlice,symbolColor=BlackARGB,symbolSize=5,title=("Slice Global "+step),xLabel="radial",yLabel="coveredArea")
                    .updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                    .updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
                //save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix+ "soaSlicePlot" + s".$step.png", width = width, height = height))
                val updaterGlobSlice = if (display) Some(SwingRenderer(globSlice, width, height, true)) else None
                println("Done drawing Global Slice Step # " + step)

                //Grid Plot
                //
                val topRow = Seq(globSurfStyle,soaSurfStyle,pwpsSurfStyle)
                val bottomRow = Seq(globSliceStyle, soaSliceStyle, pwpsSliceStyle)
                val styles = Seq(topRow, bottomRow)
                val grid = Plot.gridNN(styles)
                .withModifiedAxis[NumericAxis]("y","y2",_.updatedName("Normalized Optical Depth"))
                .updatedStyleYAxis("y2", 1, 0)
                .updatedStyleYAxis("y2", 1, 1)
                .updatedStyleYAxis("y2", 1, 2)
                .updatedAxis[NumericAxis]("x", _.min(-2.0/(3*31)-15.0/139380).max(-2.0/(3*31)+3*15.0/139380).numberFormat("%1.4f")
                    .updatedName("Radial").spacing(1e-4))
                .updatedAxis[NumericAxis]("y", _.updatedName("Azimuthal"))
                val updaterGrid = if (display) Some(SwingRenderer(grid, width, height, true)) else None
                save.foreach(prefix => SwingRenderer.saveToImage(grid, prefix+".png", width = width, height = height))
            }
            else{
                //Grid Plot
                //
                val topRow = Seq(soaSurfStyle,pwpsSurfStyle)
                val bottomRow = Seq(soaSliceStyle, pwpsSliceStyle)
                val styles = Seq(topRow, bottomRow)
                val grid = Plot.gridNN(styles)
                .withModifiedAxis[NumericAxis]("y","y2",_.updatedName("Normalized Optical Depth"))
                .updatedStyleYAxis("y2", 1, 0)
                .updatedStyleYAxis("y2", 1, 1)
                .updatedAxis[NumericAxis]("x", _.min(-2.0/(3*31)-15.0/139380).max(-2.0/(3*31)+3*15.0/139380).numberFormat("%1.4f")
                    .updatedName("Radial").spacing(1e-4))
                .updatedAxis[NumericAxis]("y", _.updatedName("Azimuthal"))
                val updaterGrid = if (display) Some(SwingRenderer(grid, width, height, true)) else None
                save.foreach(prefix => SwingRenderer.saveToImage(grid, prefix+".png", width = width, height = height))
            }
        }
    }

    def getRealAvg(data: Seq[Double], cutoffRatio: Double): Double = {
        val max = data.max
        val filtered = data.filter(d => d > cutoffRatio*max)
        filtered.sum/filtered.length
    }

    def getTau(particles: IndexedSeq[Particle], numBinsX: Int, numBinsY: Int): (Array[Double], Array[Double], Array[Double]) = {
        val xValues = particles.map(_.x)
        val yValues = particles.map(_.y)
        val xMin = xValues.min
        val xMax = xValues.max
        val yMin = yValues.min
        val yMax = yValues.max
        val xSpacing = (xMax-xMin)/numBinsX
        val ySpacing = (yMax-yMin)/numBinsY

        val xBins = Array.ofDim[Double](numBinsX*numBinsY)//(xMin + xSpacing/2 to xMax - xSpacing/2 by xSpacing).toSeq
        val yBins = Array.ofDim[Double](numBinsX*numBinsY)//(yMin + ySpacing/2 to yMax - ySpacing/2 by ySpacing).toSeq
        for(i <- 0 until numBinsX){
            for(j <- 0 until numBinsY){
                val x0 = xMin+i*xSpacing
                val y0 = yMin+j*ySpacing
                xBins(i*numBinsY+j) = x0+xSpacing/2
                yBins(i*numBinsY+j) = y0+ySpacing/2
            }
        }

        val coveredAreas = Array.fill[Double](numBinsX*numBinsY)(0.0)
        for(p <- particles){
            var i = 0
            var counted = false
            while(i < numBinsX && !counted){
                var j = 0
                while(j < numBinsY && !counted){
                    val x0 = xMin+i*xSpacing
                    val y0 = yMin+j*ySpacing
                    if(p.x >= x0 && (p.x < x0+xSpacing || i == numBinsX-1 && p.x == x0+xSpacing) && p.y >= y0 && (p.y < y0+ySpacing || j == numBinsY-1 && p.y == y0+ySpacing)){
                        coveredAreas(i*numBinsY+j) += math.Pi * math.pow(p.rad,2) / (xSpacing*ySpacing)
                        counted = true
                    }
                    j += 1
                }
                i += 1
            }
        }
        (xBins, yBins, coveredAreas)
    }

    def getSurfDensitySlice(particles: IndexedSeq[Particle], numBins: Int, pct: Double): (Array[Double], Array[Double]) = {
        val xValues = particles.map(_.x)
        val yValues = particles.map(_.y)
        val yMin = yValues.min
        val yMax = yValues.max
        val yLowBound = (0.5-pct/2)*yMax + (0.5+pct/2)*yMin
        val yTopBound = (0.5+pct/2)*yMax + (0.5-pct/2)*yMin
        val xMin = xValues.min
        val xMax = xValues.max
        val xSpacing = (xMax-xMin)/numBins

        val xBins = Array.ofDim[Double](numBins)//(xMin + xSpacing/2 to xMax - xSpacing/2 by xSpacing).toSeq
        for(i <- 0 until numBins){
            val x0 = xMin+i*xSpacing
            xBins(i) = x0+xSpacing/2
        }

        val coveredArea = Array.fill[Double](numBins)(0.0)
        for(p <- particles){
            var i = 0
            var counted = false
            while(i < numBins && !counted){
                val x0 = xMin+i*xSpacing
                if(p.y < yTopBound && p.y > yLowBound && p.x >= x0 && (p.x < x0+xSpacing || i == numBins-1 && p.x == x0+xSpacing)){
                    coveredArea(i) += (math.Pi * math.pow(p.rad,2)) / ((yTopBound-yLowBound)*xSpacing)
                    counted = true
                }
                i += 1
            }
        }
        (xBins, coveredArea)
    }
}