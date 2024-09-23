package chariklosims

import java.io.File
import occultations.PlotOccultation
import occultations.StackPlotOccultations.createStackPlot
import swiftvis2.plotting.renderer.SwingRenderer
import data.CartAndRad
import util.Particle

object DPS2024PosterPlots {
  val workDir = new File("/home/mlewis/Documents/Research/DPS2024/")
  val particleCounts = Seq("40000", "40500")
  val R0 = 335.14
  val xOffset = -0.1666666666
  val xRange = Some((-100.0, 100.0))

  def main(args: Array[String]): Unit = {
    val allFiles = workDir.listFiles()
    for (count <- particleCounts) {
      val countFiles = allFiles.filter(_.getName().contains(count))
      val (discOccFiles, simOccFiles) = countFiles.filter(_.getName().contains("occultation")).sortBy(_.getName()).partition(_.getName.contains("-disc"))
      val cartAndRadFile = countFiles.filter(_.getName().contains("CartAndRad")).head
      val cartAndRadData = CartAndRad.read(cartAndRadFile)
      makePlots(simOccFiles, count, cartAndRadData, "-sim")
      makePlots(discOccFiles, count, cartAndRadData, "-disc")
    }
  }

  def makePlots(occFiles: Array[File], count: String, cartAndRadData: IndexedSeq[Particle], ext: String): Unit = {
    val data = occFiles.map(file => PlotOccultation.readOccultationFile(file.getAbsolutePath()))
    val stackPlot = createStackPlot(data.map(_._1.toArray), R0, xRange, xOffset)
    SwingRenderer.saveToImage(stackPlot, new File(workDir, s"stackPlot$count$ext.png").getAbsolutePath(), "PNG", 3000, 3000)
    for (((occData, beamSize), occFile) <- data.zip(occFiles)) {
      val outFileName = occFile.getName().drop(11).dropRight(4)
      val occPlot = PlotOccultation.plotSample(R0, beamSize, xOffset, occData.toArray, cartAndRadData, 2000, Some((-40, 40)))
      SwingRenderer.saveToImage(occPlot, new File(workDir, s"fullPlot$outFileName.png").getAbsolutePath(), "PNG", 8000, 2000)
    }
  }
}
