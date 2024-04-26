package occultations

import java.io.File
import org.apache.commons.math3.distribution.PoissonDistribution
import SynthOccultations._
import java.io.PrintWriter

object CharikloOccultations {
  def main(args: Array[String]): Unit = {
    val (singleSample, args2) : (Option[Int], Array[String]) = {
      val ind = args.indexOf("-singleSample")
      if (ind < 0) {
        (None, args)
      } else {
        (Some(args(ind + 1).toInt), args.patch(ind, Nil, 2))
      }
    }
    
    val (sampleCap, simulationDirPath): (Int, String) = {
      val ind = args2.indexOf("-sampleCap")
      if (ind < 0) {
        println("Occulting all sample")
        (new File(args2(0)).list().length, args2(0))
      } else {
        println("Occulting over at most " + args2(ind + 1) + " sample(s)")
        (args2(ind + 1).toInt, args2(ind + 2))
      }
    }

    val simulationDir = new File(simulationDirPath)
    if (!simulationDir.exists()) {
      println("You need to specify a directory that exists.")
      sys.exit(0)
    }
    val FileRegex = """CartAndRad\.(\d+)\.bin""".r
    val simulations = singleSample match {
      case None => simulationDir.list()
      case Some(step) => simulationDir.list().filter(fileName => fileName match {
        case FileRegex(fileStep) => step == fileStep.toInt
        case _ => false
      })
    }

    //Occultation data
    //Chariklo data
    val r0: Double = 400
    //Star data
    val star = MeasurementDetails(
      "α Vir (8) I",
      "2005-141",
      90,
      90,
      90,
      400,
      400,
      3600,
      6345
    )
    val poissonDist = new PoissonDistribution(star.i0)
    val cutTheta = 0.0 // Currently radial
    val phi = 90 * math.Pi/180
    val beamSize = 0.25 / r0
    //doing a single cut
    val cutSpread = 10 / r0

    val corruptSampleLog = new PrintWriter(
      new File(simulationDir, "corruptSample.txt")
    )
    var currentSample = 0
    //loop through each sample until reach the sample cap number and generate a corresponding occultation.txt file
    for ((sim @ FileRegex(step)) <- simulations;
         if currentSample < sampleCap) {
      println(
        "Occulting " + (currentSample + 1).toString() + "(th) file: " + sim
      )

      //scan length is the integration distance: 0.5
      // val scanLength = 0.5 / star.duration / r0 / 1000
      // val scanLength = 0.5 / star.duration / 1000
      val scanLength = 0.5 / r0 //TODO: Dr. Lewis please look at this and see what scan length we would need since you originally had:
      //val scanLength = (star.rmax - star.rmin) / star.duration / r0 / 1000 // Length in R_0 for a millisecond

      val scans = collection.mutable.Buffer[Scan]()
      var faulty = false
      //TODO: Dr. Lewis please also look at scans.length to see why it needs to be < 100 cause you originally had <1000
      while (scans.length < 100 && !faulty) {
        try {
          val particles = data.CartAndRad.read(new File(simulationDir, sim))
          val binned = binParticles(particles)
          val (zmin, zmax) = {
            val sorted = particles.map(_.z).sorted
            (sorted(100), sorted(sorted.length - 100))
          }
          //get the median x, y of particles
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
          //TODO: why 2000?
          println("Scans length = " + scans.length)
        } catch {
          case ex: java.io.IOException => {
            println(
              "Problem reading " + new File(
                simulationDir,
                sim
              )
            )
            faulty = true
          }
          case _: Throwable => {
            corruptSampleLog.println(step)
            faulty = true
          }
        }
      }
      if (!faulty) {
        val outFilename = "occultation" + step + ".txt"
        val pw = new PrintWriter(
          new File(simulationDir, outFilename)
        )
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
      }
      currentSample += 1
    }
    corruptSampleLog.flush()
  }
}
