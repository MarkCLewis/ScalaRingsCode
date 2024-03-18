package occultations

import java.io.File
import org.apache.commons.math3.distribution.PoissonDistribution
import SynthOccultations._
import java.io.PrintWriter

object CharikloOccultations {
  def main(args: Array[String]): Unit = {
    val simulationDir = new File(args(0))
    val simulations = simulationDir.list()

    //TODO: verify simulation directory exists

    var count = 0
    val r0: Double = 400
    val star = MeasurementDetails(
      "α Vir (8) I",
      "2005-141",
      17.2,
      10,
      80,
      390,
      900000000,
      2546,
      479000
    )

    val FileRegex = """CartAndRad\.(\d+)\.bin""".r

    for ((sim @ FileRegex(step)) <- simulations; if count < 1) {
      println("got here")
      val poissonDist = new PoissonDistribution(star.i0 / 1000)
      val cutTheta = 0.0 // Currently radial
      val phi = (star.phiMin + star.phiMax) * 0.5 * math.Pi / 180 // Decide how to pick this better

      val beamSize = 0.01 / r0
      val cutSpread = 0.3 / r0

      //how did this come to be, it determines how much occulting we do and it uses all the rs
      val scanLength = (star.rmax - star.rmin) / star.duration / r0 / 1000 // Length in R_0 for a millisecond

      println(scanLength)

      val scans = collection.mutable.Buffer[Scan]()
      val pw = new PrintWriter(
        new File(simulationDir, "testoccultation.txt")
      )

      while (scans.length < 1000 && new File(
               simulationDir,
               sim
             ).exists()) {
        try {
          val particles = data.CartAndRad.read(new File(simulationDir, sim))
          val binned = binParticles(particles)

          val (zmin, zmax) = {
            val sorted = particles.map(_.z).sorted
            (sorted(100), sorted(sorted.length - 100))
          }
          val (cx, cy) = particles
            .sortBy(_.x)
            .apply(particles.length / 2)
            .x -> particles
            .sortBy(_.y)
            .apply(particles.length / 2)
            .y

          scans ++= multipleCuts(
            cx,
            cy,
            phi,
            star.B * math.Pi / 180,
            cutTheta,
            // scanLength,
            300,
            0.0,
            beamSize,
            zmax - zmin,
            binned,
            poissonDist.sample,
            cutSpread,
            2000
          ).flatten
          println("Scans length = " + scans.length)
        } catch {
          case ex: java.io.IOException =>
            println(
              "Problem reading " + new File(
                simulationDir,
                sim
              )
            )
        }
      }
      pw.println(star)
      pw.println(sim)
    }

  }
}
