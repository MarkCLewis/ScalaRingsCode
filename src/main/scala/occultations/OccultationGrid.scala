package occultations

import java.io.File
import org.apache.commons.math3.distribution.PoissonDistribution
import SynthOccultations._
import util.Particle
import java.io.PrintWriter
import collection.JavaConverters._

/**
 * This program is intended to do synthetic occultations across a grid of values for the parameters B and phi.
 */
object OccultationGrid {
  def main(args: Array[String]): Unit = {
    if(args.length < 1) {
      println("Specify the directories that you want to process.")
      sys.exit(0)
    }
    val DirRegex = """.*a=([\d.]+):q=([\d.]+):min=([\d.eE-]+):max=([\d.eE-]+):rho=([\d.]+):\w+=([\d.]+)(.*)""".r
    val FileRegex = """CartAndRad\.(\d+)\.bin""".r
    val directories = args
    println("Directories = "+directories.mkString(", "))

    val simulations = (for (dirStr @ DirRegex(r0Str, qStr, minStr, maxStr, rhoStr, sigmaStr, rest) <- directories) yield {
      println(dirStr)
      val dir = new File(dirStr)
      val num = {
        val files = dir.list
        (for (f @ FileRegex(num) <- files) yield num.toInt).reduceLeftOption((n1, n2) => if (n1 > n2) n1 else n2)
      }
      val r0 = r0Str.toDouble
      val q = qStr.toDouble
      val radMin = minStr.toDouble
      val radMax = maxStr.toDouble
      val rho = rhoStr.toDouble
      val sigma = sigmaStr.toDouble
      num.filter(_ >= 10000).map(n => Simulation(dir, n, r0, q, radMin, radMax, rho, sigma, rest))
    }).flatten

    simulations.foreach(println)

    val pw = new PrintWriter(new File("occultations.txt"))

    val i0 = 500000
    for (sim <- simulations) {
      val beamSize = 0.01 / sim.r0
      val cutSpread = 0.3 / sim.r0
      val scanLength = 1e-2 / sim.r0 // Length in R_0 for a millisecond
      val maxStep = sim.maxFileNum 
      for (b <- 5 to 70 by 5; phiDegrees <- 0 to 170 by 10) {
        val poissonDist = new PoissonDistribution(i0 / 1000)
        val cutTheta = 0.0 // Currently radial
        val phi = phiDegrees * math.Pi/180 // Decide how to pick this better
        var step = sim.maxFileNum
        val scans = collection.mutable.Buffer[Scan]()
        while (scans.length < 1000 && step >= 10000 && new File(sim.dir, "CartAndRad." + sim.maxFileNum + ".bin").exists()) {
          try {
            val particles = data.CartAndRad.read(new File(sim.dir, "CartAndRad." + sim.maxFileNum + ".bin"))
            val binned = binParticles(particles)
            println(s"Using step $step with ${particles.length} particles.")
            val (zmin, zmax) = {
              val sorted = particles.map(_.z).sorted
              (sorted(100), sorted(sorted.length - 100))
            }
            scans ++= multipleCuts(0, 0, phi, b * math.Pi/180, cutTheta, scanLength,
              0.0, beamSize, zmax - zmin, binned, poissonDist.sample, cutSpread).flatten
            println("Scans length = "+scans.length)
          } catch {
            case ex:java.io.IOException =>
              println("Problem reading "+new File(sim.dir, "CartAndRad." + sim.maxFileNum + ".bin"))
          }
          step -= 1000
        }
        pw.println(s"B = $b, phi = $phi")
        pw.println(sim)
        pw.println("Used orbits "+(step+1000)/1000.0+" to "+maxStep/1000.0)
        pw.println("Index\tPhotons\tTrans\tFraction\tStart X\tStart Y\tEnd X\tEnd Y")
        for ((scan, i) <- scans.zipWithIndex) {
          pw.println(s"$i\t${scan.photons.length}\t${scan.photons.count(!_.hit)}\t${scan.intensity}\t${scan.sx}\t${scan.sy}\t${scan.ex}\t${scan.ey}")
        }
      }
    }

  }
}