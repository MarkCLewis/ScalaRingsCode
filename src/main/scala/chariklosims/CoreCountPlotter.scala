package chariklosims

import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer
import swiftvis2.plotting.styles.ScatterStyle

case class CoreData(step: Int, group: Int, count: Int)

object CoreCountPlotter {
  def stepsActive(group: Int, stepsCoreDataMap: Map[Int, IndexedSeq[Int]]): Int = {
    stepsCoreDataMap(group).last - stepsCoreDataMap(group).head
  }

  def main(args: Array[String]): Unit = {
    val coreFile = io.Source.fromFile(args(0))
    val stepsPerOrbit = args(1).toInt
    val lines = coreFile.getLines().toArray
    coreFile.close()
    val coreLine = """(\d+), (\d+), IndexAndStep\(\d+,\d+\), (\d+)""".r
    val stepsCoreDataMap = (for (coreLine(step, group, cnt) <- lines; if cnt != "0") yield {
      CoreData(step.toInt, group.toInt, cnt.toInt)
    }).groupBy(_.step).map(t => t._1 -> t._2.sortBy(_.step))
    val stepsCoreData = stepsCoreDataMap.toIndexedSeq.sortBy(_._1)
    val groupStepsMap = stepsCoreData.flatMap(t => t._2.map(_ -> t._1)).groupBy(_._1.group).map(t => t._1 -> t._2.map(_._1.step).sorted)

    val x = stepsCoreData.map(_._1.toDouble / stepsPerOrbit)   
    val y = stepsCoreData.map(_._2.length.toDouble)
    val y2 = stepsCoreData.map(t => t._2.count(cd => stepsActive(cd.group, groupStepsMap) > 5000))
    val y3 = stepsCoreData.map(t => t._2.map(_.count).sum)
    val y4 = stepsCoreData.map(t => t._2.filter(cd => stepsActive(cd.group, groupStepsMap) > 5000).map(_.count).sum)
    val plot = Plot.scatterPlot(x, y3, "Moonlet Count", "Orbits", "Count", 10.0, BlackARGB)
    SwingRenderer(plot, 1000, 1000, true)
  }
}
