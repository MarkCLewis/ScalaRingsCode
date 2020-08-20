package simprocessing

import data.FixedBinned
import java.io.File
import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer

import java.io._ 
import scala.io.Source
import scala.collection.mutable


//this should read in a FixedBinned file and output a file containing the locations of all the maxima in each slice
object FixedBinnedPeakFinder {
	def main(args: Array[String]): Unit = {
		val usingRealFile = true
		if (usingRealFile) {
			if (args.contains("-help") || args.length < 1) {
				println("Arguments:")
				println("\t-dir path: path to directory, defaults to current directory")
				println("\t-width #: width of window/image in pixels, defaults to 1000")
				println("\t-height #: height of window/image in pixels, defaults to 1000")
				println("\t-display: tells if the image should be displayed in a window")
				println("\t-save prefix: tells if images should be saved and gives prefix")
				println("\t-stepRange #:the range of steps you want to analyze as 2 integers [). Defaults to the whole file")
				println("\t-azMin #:the minimum azimuthal value to display in the surface plot")
				println("\t-azMax #:the maximum azimuthal value to display in the surface plot")
				sys.exit()
			}
			val dir = new File(args.sliding(2).find(_(0) == "-dir").map(_(1)).getOrElse("."))
			val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
			val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
			val display = args.contains("-display")
			val save = args.sliding(2).find(_(0) == "-save").map(_(1))
			val stepRange = args.sliding(3).find(_(0) == "-stepRange").map(arg => (arg(1).toInt, arg(2).toInt))
			val azMin = args.sliding(2).find(_(0) == "-azMin").map(_(1).toDouble).getOrElse(Double.MinValue)
			val azMax = args.sliding(2).find(_(0) == "-azMax").map(_(1).toDouble).getOrElse(Double.MaxValue)

			val (categories, allFixedBins) = FixedBinned.read(new File(dir, "FixedBinned.bin").getAbsolutePath())
			val sgn = math.signum(allFixedBins(1)(0)(0) - allFixedBins(0)(0)(0))
			val fixedBins = allFixedBins.filter(col => col(0)(0) >= azMin && col(0)(0) <= azMax)
			if(true) categories.zipWithIndex.foreach(println)

			println(stepRange.get)
			println("FixedBins Length: " + fixedBins.length)

			val (start, end) = stepRange.getOrElse((0,fixedBins.length))
			writeFile("extremeLocations."+start+"-"+(end-1)+".txt", Seq(""+(end-start),"\n"))
			for ((step, index) <- fixedBins.slice(start,end).zipWithIndex) {
				val yValues = step.map { col => col(1) }
				val tauValues = step.map { col => col(4) }
				val window = 20
				val extrema = MinMaxFinder.getLocalExtrema(yValues,tauValues,window).filter(_.isMax)
				val locations = extrema.map { e => e.x}
				val extremeValues = extrema.map { e => e.y}

				// val lines = Array.ofDim[String](1+yValues.size*2)
				// lines(0) = yValues.size+"\n"
				// for(i <- 0 until yValues.size){
				// 	lines(2*i+1) = yValues(i)+"\n"
				// 	lines(2*i+2) = ""+tauValues(i)+"\n"
				// }
				// writeFile("y_tau_index"+slice(0)+"-"+slice(1)+".txt",lines.toSeq)

				val lines = Array.ofDim[String](extrema.length+2)
				lines(0) = start+index + "\n"
				lines(1) = extrema.length + "\n"
				for (i <- 2 until lines.length){
					lines(i) = ""+locations(i-2)+"\n" 
				}
				writeFile("extremeLocations."+start+"-"+(end-1)+".txt", lines)
				
				if(save.nonEmpty || display){
					val plot = Plot.scatterPlots(Seq((yValues,tauValues,BlackARGB,5),(locations,extremeValues,RedARGB,10)),title=("Step number "+index),xLabel="radial",yLabel="tau")
					.updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
					.updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f"))))
					save.foreach(prefix => SwingRenderer.saveToImage(plot, prefix + s".$index.png", width = width, height = height))
					println("Done drawing Step # " + index)

					val updater = if (display) Some(SwingRenderer(plot, width, height, true)) else None
					//SwingRenderer.saveToImage(plot, prefix + s".$index.png", width = width, height = height)
					println("Done drawing Step # " + index)
				}
			}
		}
		else {
			val data = readTestingFile("y_tau_index15000-15010.txt")
			val prefix = "textFileDataTest"
			val display = true
			val width = 1000
			val height = 1000
			for(((radValues,tauValues), index) <- data.zipWithIndex){
				val window = 20
				val extrema = MinMaxFinder.getLocalExtrema(radValues,tauValues,window).filter(_.isMax)
				val locations = extrema.map { e => e.x}
				val extremeValues = extrema.map { e => e.y}

				val radStep = (radValues(radValues.size-1) - radValues(0)) / radValues.size
				println("radial step = "+radStep)

				// val parabolaDrawingWindow = 30

				// val parabolasX = Array.ofDim[Double](extrema.size*parabolaDrawingWindow)
				// val parabolasY = Array.ofDim[Double](extrema.size*parabolaDrawingWindow)
				// for(i <- 0 until extrema.size){
				// 	val ef = extrema(i)
				// 	val func = (z:Double) => ef.a*z*z + ef.b*z + ef.c
				// 	val offsetValue = ef.location - radStep*parabolaDrawingWindow/2
				// 	for(j <- 0 until parabolaDrawingWindow){
				// 		parabolasX(i*parabolaDrawingWindow+j) = offsetValue + j*radStep
				// 		parabolasY(i*parabolaDrawingWindow+j) = func(parabolasX(i*parabolaDrawingWindow+j))
				// 	}
				// }

				// val lines = Array.ofDim[String](1+yValues.size*2)
				// lines(0) = yValues.size+"\n"
				// for(i <- 0 until yValues.size){
				// 	lines(2*i+1) = yValues(i)+"\n"
				// 	lines(2*i+2) = ""+tauValues(i)+"\n"
				// }
				// writeFile("y_tau_index"+slice(0)+"-"+(slice(1)-1)+".txt",lines.toSeq)

				val lines = Array.ofDim[String](extrema.length+1)
				lines(0) = index + "\n"
				for (i <- 1 until lines.length){
					lines(i) = ""+locations(i-1)+"\n" 
				}
				writeFile("testOutput.txt", lines)
				
				val plot = Plot.scatterPlots(Seq((radValues,tauValues,BlackARGB,5),(locations,extremeValues,RedARGB,10)/*,(parabolasX,parabolasY,RedARGB,5)*/),title=("Step number "+index),xLabel="radial",yLabel="tau")
				.updatedAxis[NumericAxis]("x", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.4f"))))
				.updatedAxis[NumericAxis]("y", axis => axis.copy(tickLabelInfo = axis.tickLabelInfo.map(_.copy(numberFormat = "%1.2f"))))
				
				
				val updater = if (display) Some(SwingRenderer(plot, width, height, true)) else None
				//SwingRenderer.saveToImage(plot, prefix + s".$index.png", width = width, height = height)
				println("Done drawing Step # " + index)
			}
		}
	}

	/**
	* write a `Seq[String]` to the `filename`.
	*/
	def writeFile(filename: String, lines: Seq[String]): Unit = {
		val file = new File(filename)
		val bw = new BufferedWriter(new FileWriter(file,true))
		for (line <- lines) {
			bw.write(line)
		}
		bw.close()
	}

	def readTestingFile(filename: String): Seq[(Seq[Double], Seq[Double])] = {
		val bufferedSource = Source.fromFile(new java.io.File(filename))
		val lines = bufferedSource.getLines()
		val numPlots = lines.next.toInt
		//print(numPlots)
		val ret = mutable.ArrayBuffer.empty[(Seq[Double], Seq[Double])]
		for(i <- 0 until numPlots){
			val len = lines.next.toInt
			//print(len)
			val arr1 = Array.ofDim[Double](len)
			val arr2 = Array.ofDim[Double](len)
			for(j <- 0 until len){
				arr1(j) = lines.next.toDouble
				arr2(j) = lines.next.toDouble
			}
			val pair = (arr1.toSeq, arr2.toSeq)
			ret += pair
		}
		bufferedSource.close
		ret.toSeq
	}
}