package occultations

import java.io.File

/**
 * The purpose of this application is to read in the output of the automatic occultation
 * calculations and do appropriate calculations for each of the occultations.
 */
object ProcessOccultations extends App {
  case class DataPoint(index: Int, photons: Int, transmitted: Int, fraction: Double)
  case class OccultationData(measure: MeasurementDetails, sim: Simulation, scan: Seq[DataPoint])

  val DataPointRegex = """(\d+)\s+(\d+)\s+(\d+)\s+([\d.e-]+)""".r

  val source = scala.io.Source.fromFile(args(0))
  val data = readData(source.getLines)
  source.close
  
  // Calculate stuff with the data
  println(data.length)

  def readData(lines: Iterator[String]): Seq[OccultationData] = {
    val ret = collection.mutable.Buffer[OccultationData]()
    var curMeasure = MeasurementDetails(lines.next).get
    var curSim = Simulation(lines.next).get
    var curScan = List[DataPoint]()
    for (line <- lines) {
      line match {
        case MeasurementDetails.Regex(star, date, b, phMin, phiMax, rmin, rmax, duration, i0) =>
          ret += OccultationData(curMeasure, curSim, curScan.reverse)
          curMeasure = MeasurementDetails(star, date, b.toDouble, phMin.toDouble, phiMax.toDouble, rmin.toDouble, rmax.toDouble, duration.toDouble, i0.toDouble)
          curScan = List[DataPoint]()
        case Simulation.Regex(dir, maxFileNum, r0, q, radMin, radMax, rho, sigma) =>
          curSim = Simulation(new File(dir), maxFileNum.toInt, r0.toDouble, q.toDouble, radMin.toDouble, radMax.toDouble, rho.toDouble, sigma.toDouble)
        case DataPointRegex(index, photons, transmitted, fraction) =>
          curScan ::= DataPoint(index.toInt, photons.toInt, transmitted.toInt, fraction.toDouble)
        case _ =>
      }
    }
    ret += OccultationData(curMeasure, curSim, curScan)
    ret
  }
}