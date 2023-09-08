package chariklosims

import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.Renderer

case class CoreData(step: Int, group: Int, count: Int)

object CoreCountPlotter {
  def stepsActive(group: Int, stepsCoreDataMap: Map[Int, IndexedSeq[Int]], currentStep: Int, delta: Int): Int = {
    val stepList = stepsCoreDataMap(group)
    val index = stepList.indexOf(currentStep)
    if (index == -1) 0
    else {
      var prevStep = index
      while (prevStep > 0 && stepList(prevStep) - stepList(prevStep - 1) == delta) prevStep -= 1
      var nextStep = index
      while (nextStep < stepList.length - 1 && stepList(nextStep + 1) - stepList(nextStep) == delta) nextStep += 1
      stepList(nextStep) - stepList(prevStep)
    }
  }

  def plotFromCoreData(coreFileName: String, stepsPerOrbit: Double, color: Int): Seq[ScatterStyle] = {
    val coreFile = io.Source.fromFile(coreFileName)
    val lines = coreFile.getLines().toArray
    coreFile.close()
    val delta = stepsPerOrbit.toInt / 50
    val coreLine = """(\d+), (\d+), IndexAndStep\(\d+,\d+\), (\d+)""".r
    val stepsCoreDataMap = (for (coreLine(step, group, cnt) <- lines; if cnt != "0") yield {
      CoreData(step.toInt, group.toInt, cnt.toInt)
    }).groupBy(_.step).map(t => t._1 -> t._2.sortBy(_.step))
    val stepsCoreData = stepsCoreDataMap.toIndexedSeq.sortBy(_._1)
    val groupStepsMap = stepsCoreData.flatMap(t => t._2.map(_ -> t._1)).groupBy(_._1.group).map(t => t._1 -> t._2.map(_._1.step).sorted)

    val x = stepsCoreData.map(_._1.toDouble / stepsPerOrbit)   
    val y_count = stepsCoreData.map(t => t._2.count(cd => stepsActive(cd.group, groupStepsMap, t._1, delta) > stepsPerOrbit / 3))
    val y_mass = stepsCoreData.map(t => t._2.filter(cd => stepsActive(cd.group, groupStepsMap, t._1, delta) > stepsPerOrbit / 3).map(_.count).sum / 1e3)
    Seq(
      ScatterStyle(x, y_count, symbolWidth = 5, symbolHeight = 5, colors = color, lines = Some(ScatterStyle.LineData(1, Renderer.StrokeData(1, Nil)))),
      ScatterStyle(x, y_mass, symbolWidth = 5, symbolHeight = 5, colors = color, lines = Some(ScatterStyle.LineData(1, Renderer.StrokeData(1, Nil))))
    )
  }
  
  def main(args: Array[String]): Unit = {
    val scatters = args.grouped(2).zip(MoonletPubPlot.MoonletColors).map { case (Array(fn, spo), color) => plotFromCoreData(fn, spo.toDouble, color) }.toSeq
    val font = Renderer.FontData("Ariel", Renderer.FontStyle.Plain)
    val plot = Plot.stackedGridNN(Seq(scatters.transpose), xLabel = "Orbits", yLabel = "Moonlet Count")
      .updatedAxis[NumericAxis]("x", _.updatedNumberFormat("%1.0f"))
      .withAxis("y2", NumericAxis(Some(0.0), None, None, Axis.TickStyle.Both,
          Some(Axis.LabelSettings(0.0, font, "%1.0f")), Some(Axis.NameSettings("Particles In Moonlet Count (x1000)", font)), Axis.DisplaySide.Max))
      .updatedYAxisOnPlot(0, 1, 0, "y2")
      .updatedYAxisOnPlot(0, 1, 1, "y2")
      .updatedGridBounds(b => b.copy(y = 0.02, height = 0.98))
    SwingRenderer(plot, 1000, 1000, true)
  }
}
