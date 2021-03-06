package occultations

import java.io.File

case class Simulation(
  dir:        File,
  maxFileNum: Int,
  r0:         Double,
  q:          Double,
  radMin:     Double,
  radMax:     Double,
  rho:        Double,
  sigmaOrTau: Double,
  rest:       String)

object Simulation {
  val Regex = """Simulation\((.+),(\d+),([\d.E-]+),([\d.E-]+),([\d.E-]+),([\d.E-]+),([\d.E-]+),([\d.E-]+),(.*)\)""".r

  def apply(line: String): Option[Simulation] =
    line match {
      case Regex(dir, maxFileNum, r0, q, radMin, radMax, rho, sigma, rest) =>
        Some(Simulation(new File(dir), maxFileNum.toInt, r0.toDouble, q.toDouble, radMin.toDouble, radMax.toDouble, rho.toDouble, sigma.toDouble, rest))
      case _ => None
    }
}