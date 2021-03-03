package keatonThesisFigs

import data.FixedBinned

import swiftvis2.plotting.styles._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.Renderer
import swiftvis2.plotting.Plot.GridData
import swiftvis2.plotting.renderer.SwingRenderer

import java.io._ 
import scala.collection.mutable

object NoMoonPanel {
	def main(args: Array[String]): Unit = {
        if (args.contains("-help") || args.length < 1) {
            println("Arguments:")
            println("\t-dir path: path to directory, defaults to current directory")
            println("\t-width #: width of window/image in pixels, defaults to 1000")
            println("\t-height #: height of window/image in pixels, defaults to 1000")
            println("\t-display: tells if the image should be displayed in a window")
            println("\t-save prefix: tells if images should be saved and gives prefix")
            println("\t-stepRange #: the steps you want to analyze as 3 integers. Defaults to first 3 outputs")
            sys.exit()
        }
        val dir = new File(args.sliding(2).find(_(0) == "-dir").map(_(1)).getOrElse("."))
        val width = args.sliding(2).find(_(0) == "-width").map(_(1).toInt).getOrElse(1000)
        val height = args.sliding(2).find(_(0) == "-height").map(_(1).toInt).getOrElse(1000)
        val display = args.contains("-display")
        val save = args.sliding(2).find(_(0) == "-save").map(_(1))
        val stepRange = args.sliding(4).find(_(0) == "-stepRange").map(arg => (arg(1).toInt/100, arg(2).toInt/100, arg(3).toInt/100)).getOrElse((0,1,2))

        val (categories, fixedBins) = FixedBinned.read(new File(dir, "FixedBinned.bin").getAbsolutePath())
        //val sgn = math.signum(allFixedBins(1)(0)(0) - allFixedBins(0)(0)(0))
        if(true) categories.zipWithIndex.foreach(println)

        println("stepRange",stepRange)
        println("FixedBins Length: " + fixedBins.length)

        val (first, second, third) = stepRange
        val zipped = fixedBins.slice(first,third+1).zipWithIndex
        val panelStyles = Array.ofDim[ScatterStyle](3)
        var cnt = 0
        for ((step, index) <- Seq(zipped(0), zipped(second-first), zipped(zipped.length-1))) {
            val yValues = step.map { col => col(1) } //these are really the radial coordinates
            val tauValues = step.map { col => col(4) }
            panelStyles(cnt) = ScatterStyle(yValues, tauValues, symbolWidth=5, symbolHeight=5)
            cnt += 1
        }   
        val printIndex = 100*(first)
        if(save.nonEmpty || display){
            val topRow = Seq(Seq(panelStyles(0)), Seq(panelStyles(1)), Seq(panelStyles(2)))
            val bottomRow = Seq()
            val styles = Seq(topRow)
            val grid = Plot.stackedGridNN(styles)
            .updatedAxis[NumericAxis]("x", _.updatedName("Radial").numberFormat("%1.4f"))
            .updatedAxis[NumericAxis]("y", _.min(0.0).max(0.25).updatedName("Optical Depth").numberFormat("%1.4f"))

            val updaterGrid = if (display) Some(SwingRenderer(grid, width, height, true)) else None
            save.foreach(prefix => SwingRenderer.saveToImage(grid, prefix + s".$printIndex.png", width = width, height = height))
        }
	}
}