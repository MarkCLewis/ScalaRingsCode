package chariklosims

import java.io.File
import org.apache.commons.math3.distribution.PoissonDistribution
import occultations.SynthOccultations._
import java.io.PrintWriter
import occultations.MeasurementDetails

case class OccultationSettings(name: String, B: Double, i0: Int, beamSize: Double, scanLength: Double, offLength: Double)

object Occultations {
  val sample = Seq(
    OccultationSettings("cassini", 90, 600, 0.01, (143444.0 - 73267.0) / 10191.0 / 1000.0, 0.0001),
    OccultationSettings("best", 90, 1250000, 0.25, 1.0, 0.017),
    OccultationSettings("best2", 90, 125000, 0.25, 0.1, 0.017),
    OccultationSettings("average", 90, 15400, 0.5, 10, 0.017),
    OccultationSettings("average2", 90, 1540, 0.5, 1.0, 0.017),
    OccultationSettings("bad", 90, 15400, 0.5, 48, 15),
    OccultationSettings("Danish-disc", 90, 102000, 2.18/2, 2.04, 0.00091*20.4),
    OccultationSettings("TRAPPIST-disc", 90, 793000, 2.18/2, 91.8, 1.3*20.4),
    OccultationSettings("PROMPT-disc", 90, 54800, 2.18/2, 14.28, 1.3*20.4),
    OccultationSettings("SOAR-disc", 90, 4950000, 2.18/2, 61.2, 1.5*20.4),
  )

  /* 
   * @param args should cointain the path to folder containing all the samples 
   * @param (Optional) "-singleSample Int" to process a single sample file with a step Int
   * @param (Optional) "-sampleCap Int" to only process a limited amount of sample files
   */
  def main(args: Array[String]): Unit = {
    val simulationDir = new File(args(0))
    if (!simulationDir.exists()) {
      println("You need to specify a directory that exists.")
      sys.exit(0)
    }
    val FileRegex = """CartAndRad\.(\d+)\.bin""".r
    val simulations = simulationDir.list()

    //Occultation data
    val r0: Double = 304

    var currentSample = 0
    //loop through each sample until reach the sample cap number and generate a corresponding occultation.txt file
    for ((sim @ FileRegex(step)) <- simulations) {
      println("Occulting " + (currentSample + 1).toString() + "(th) file: " + sim)

      try {
        val particles = data.CartAndRad.read(new File(simulationDir, sim))
        val binned = binParticles(particles)
        val (zmin, zmax) = {
          val sorted = particles.map(_.z).sorted
          (sorted(100), sorted(sorted.length - 100))
        }
        //get the median x, y of particles
        val (cx, cy) = particles
          .sortBy(_.x).apply(particles.length / 2).x -> 
          particles.sortBy(_.y).apply(particles.length / 2).y
        for ((OccultationSettings(name, b, i0, beamSizekm, scanLengthkm, offLengthkm), i) <- sample.zipWithIndex) {
          val B = b
          val poissonDist = new PoissonDistribution(i0)
          val cutTheta = 0.0 // Currently radial
          val phi = 90 * math.Pi/180
          val beamSize = beamSizekm / r0
          val scanLength = scanLengthkm / r0 //TODO: double-check
          val offLength = offLengthkm / r0
          val scans = collection.mutable.Buffer[Scan]()

          scans ++= syntheticOccultation(cx, cy, phi, B * math.Pi / 180, cutTheta, scanLength, offLength, beamSize, zmax - zmin, binned, poissonDist.sample, 30.0)
          println("Scans length = " + scans.length)
          val outFilename = s"occultation$step.$i.$name.txt"
          val pw = new PrintWriter(
            new File(simulationDir, outFilename)
          )
          pw.println(sim)
          pw.println("BeamSize:" + (beamSize * r0).toString())
          pw.println("Index\tPhotons\tTrans\tFraction\tstart-x\tstart-y\tend-x\tend-y")
          for ((scan, i) <- scans.zipWithIndex) {
            pw.println(s"$i\t${scan.photons.length}\t${scan.photons
              .count(!_.hit)}\t${scan.intensity}\t${scan.sx}\t${scan.sy}\t${scan.ex}\t${scan.ey}")
          }
          pw.flush()
        }
      } catch {
        case ex: java.io.IOException => {println("Problem reading " + new File(simulationDir, sim))}
      }
      currentSample += 1
    }
  }
}