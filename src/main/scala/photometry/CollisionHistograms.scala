package photometry

import java.io.File
import data.CartAndRad
import util.GCCoord
import swiftvis2.plotting._
import swiftvis2.plotting.styles.HistogramStyle
import swiftvis2.plotting.renderer.SwingRenderer

/**
  * This 
  */
object CollisionHistograms extends App {
  val step = 1000
  val num = step.toString()
  val dataName = s"CartAndRad.$step.bin"
  val dataDir = new File("/home/mlewis/workspaceResearch/RegolithImpactSims/ImpactTesting/Reg/")
  val labelFormat = "%1.3e"

  for (simDir <- dataDir.listFiles(); if simDir.isDirectory() && new File(simDir, dataName).exists()) {
    val imageDir = new File(simDir, "plots")
    imageDir.mkdirs()
    val allData = CartAndRad.read(new File(simDir, dataName))
    val bigs = allData.sortBy(-_.rad).take(2).toSet
    val data = allData.filter(p => bigs.forall(b => b.distance(p) > 2*b.rad))
    if (data.length >= allData.length*0.25) {
      val gcdata = data.map(GCCoord.apply)
      val xData = gcdata.map(_.X)
      val xMin = xData.min
      val xMax = xData.max
      val xHist = HistogramStyle.fromData(xData, (0 until 100).map(_ * (xMax-xMin) / 100 + xMin), BlackARGB, true)
      val eData = gcdata.map(_.e)
      val eMin = eData.min
      val eMax = eData.max
      val eHist = HistogramStyle.fromData(eData, (0 until 100).map(_ * (eMax-eMin) / 100 + eMin), RedARGB, true)
      val phiData = gcdata.map(_.phi)
      val phiMin = phiData.min
      val phiMax = phiData.max
      val phiHist = HistogramStyle.fromData(phiData, (0 until 100).map(_ * (phiMax-phiMin) / 100 + phiMin), GreenARGB, true)
      val iData = gcdata.map(_.i)
      val iMin = iData.min
      val iMax = iData.max
      val iHist = HistogramStyle.fromData(iData, (0 until 100).map(_ * (iMax-iMin) / 100 + iMin), BlueARGB, true)
      val plot = Plot.simple(xHist, simDir.getName(), "X", "Count")
        .updatedAxis[NumericAxis]("x", _.numberFormat(labelFormat))
        .withModifiedAxis[NumericAxis]("x", "e", axis => axis.updatedName("e"))
        .withModifiedAxis[NumericAxis]("x", "phi", axis => axis.updatedName("phi"))
        .withModifiedAxis[NumericAxis]("x", "i", axis => axis.updatedName("i"))
        .updatedPlotGrid(_.withColumn(1))
        .updatedPlotGrid(gd => gd.withStyle(eHist, "e", "y", 0, 1))
        .updatedPlotGrid(_.withColumn(2))
        .updatedPlotGrid(gd => gd.withStyle(phiHist, "phi", "y", 0, 2))
        .updatedPlotGrid(_.withColumn(3))
        .updatedPlotGrid(gd => gd.withStyle(iHist, "i", "y", 0, 3))
      SwingRenderer.saveToImage(plot, s"${imageDir.getAbsolutePath()}/hists.${"0"*(5-num.length)+num}.png", width = 1600, height = 800)
    }
  }
}