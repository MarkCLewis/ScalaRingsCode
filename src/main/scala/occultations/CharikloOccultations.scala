package occultations

import java.io.File
import org.apache.commons.math3.distribution.PoissonDistribution
import SynthOccultations._
import java.io.PrintWriter

object CharikloOccultations {
  def main(args: Array[String]): Unit = {
    val simulationDir = new File(args(0))
    if (!simulationDir.exists()) {
      println("You need to specify a directory that exists.")
      sys.exit(0)
    }
    val simulations = simulationDir.list()

    var numSample = 0
    val sampleCap = 1

    val r0: Double = 400
    val star = MeasurementDetails(
      "α Vir (8) I",
      "2005-141",
      90,
      90,
      90,
      400,
      400,
      36,
      6345
    )

    val FileRegex = """CartAndRad\.(\d+)\.bin""".r
    val pw = new PrintWriter(
      new File(simulationDir, "testoccultation.txt")
    )

    for ((sim @ FileRegex(step)) <- simulations; if numSample < sampleCap) {

      //dont divide by 1000??? why did I leave this comment here
      val poissonDist = new PoissonDistribution(star.i0 / 1000)
      val cutTheta = 0.0 // Currently radial

      val phi = (star.phiMin + star.phiMax) * 0.5 * math.Pi / 180 // Decide how to pick this better
      val beamSize = 0.25 / r0
      //we are not doing multiple cuts, you can set distance from base cut by 10 that's best so you dont add more, it's something
      val cutSpread = 7 / r0

      //scan length is the integration distance: 0.5
      // val scanLength = (star.rmax - star.rmin) / star.duration / r0 / 1000 // Length in R_0 for a millisecond
      val scanLength = 0.5 / star.duration / star.duration / r0 / 1000
      val scans = collection.mutable.Buffer[Scan]()

      println("beam size" + beamSize.toString())

      while (scans.length < 100 && new File(
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
            scanLength,
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
      pw.println(
        "Index\tPhotons\tTrans\tFraction\tstart-x\tstart-y\tend-x\tend-y"
      )
      for ((scan, i) <- scans.zipWithIndex) {
        pw.println(s"$i\t${scan.photons.length}\t${scan.photons
          .count(!_.hit)}\t${scan.intensity}\t${scan.sx}\t${scan.sy}\t${scan.ex}\t${scan.ey}")
      }
      pw.flush()

      numSample += 1
    }

  }
}
