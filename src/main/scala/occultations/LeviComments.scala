package occultations

import java.io.File
import org.apache.commons.math3.distribution.PoissonDistribution
import SynthOccultations._
import util.Particle
import java.io.PrintWriter
import collection.JavaConverters._

object LeviComments {
  //args: -azimuthal*OPTIONAL -starFile*OPTIONAL (starFile csv name, star as in occulted star?)
  //simulationData*(//"/home/mlewis/Rings/JoshCDAP15-17/") -centerOnMedians
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println(
        "Specify the directory above the simulation directory that you want to process."
      )
      println("Optional Arguments:")
      println(
        " -azimuthal: transposes x and y in the data so azimuthal cuts are made."
      )
      println(" -starFile: delimited file of star data.")
      sys.exit(0)
    }

    //read to check if azimuthal is a thing
    val (azimuthal, args2) = {
      val (a1, a2) = args.partition(_ == "-azimuthal")
      (a1.nonEmpty, a2)
    }

    //read the star data from arg2 by opening some csv file or using the baseStars if not specified
    val (starData, args3) = {
      val ind = args2.indexOf("-starFile")
      //if doesn't specify then use the base stars measurements
      if (ind < 0) (MeasurementDetails.baseStars, args2)
      else {
        val source = scala.io.Source.fromFile(args2(ind + 1))
        val sd = source.getLines
          .flatMap(line => MeasurementDetails.fromCSV(line))
          .toVector
        val sd2 =
          // if it's azimuthal get the star data but with phiMin 0, phiMax 0
          //LOGIC?
          if (azimuthal) sd.map(md => md.copy(phiMin = 0.0, phiMax = 0.0))
          else sd
        source.close()
        (sd2, args2.patch(ind, Nil, 2))
      }
    }
    println(starData)

    val simDataDirectory = args3(0) //"/home/mlewis/Rings/JoshCDAP15-17/"
    val simDataDirectoryFile = new File(simDataDirectory)

    if (!simDataDirectoryFile.exists()) {
      println("You need to specify a directory that exists.")
      sys.exit(0)
    }
    val centerOnMedians = args.contains("-centerOnMedians")
    val DirRegex =
      """a=([\d.]+):q=([\d.]+):min=([\d.eE-]+):max=([\d.eE-]+):rho=([\d.]+):\w+=([\d.]+)(.*)""".r
    val FileRegex = """CartAndRad\.(\d+)\.bin""".r

    val directories = simDataDirectoryFile.list

    //go to a folder like testChariklo and loop through all the folders within in, matching their name
    val simulations = (for (dirStr @ DirRegex(
                              r0Str,
                              qStr,
                              minStr,
                              maxStr,
                              rhoStr,
                              sigmaStr,
                              rest
                            ) <- directories) yield {

      //go inside the folder which contains all the cart and Rad Bin
      val dir = new File(simDataDirectoryFile, dirStr)
      val num = {
        val files = dir.list
        //parse each CartAndRad abd extract number
        (for (f @ FileRegex(num) <- files)
          //looping through all the num from all the CartAndRad folder and compiling it to a list then returning the Some() of the biggest number
          yield num.toInt).reduceLeftOption((n1, n2) => if (n1 > n2) n1 else n2)
      }
      println(num);
      val r0 = r0Str.toDouble
      val q = qStr.toDouble
      val radMin = minStr.toDouble
      val radMax = maxStr.toDouble
      val rho = rhoStr.toDouble
      val sigma = sigmaStr.toDouble
      //call the simulation case class constructor on the value in the Some()
      num.map(n => Simulation(dir, n, r0, q, radMin, radMax, rho, sigma, rest))
    }).flatten
    //IMPORTANT
    //simulations now contain a list of Simulation(directory name(a=400:...), biggest Cart and Rad num, 400, numbers...)

    //r0 important: distance of ring from central body.
    //400 km

    // 1000 measurements per second.
    // Calculate variance for 1000 measurements
    // Check both uniform and non-uniform photon distributions
    // Use number of photons with a mean of i0/1000 taken from a Poisson distribution

    //creating a mutable hashmap
    val dataSets = new java.util.WeakHashMap[
      (Simulation, Int),
      (IndexedSeq[Particle], BinData)
    ]().asScala

    val pw = new PrintWriter(new File(simDataDirectoryFile, "occultations.txt"))

    // for (star <- starData) {
    val star = starData(0)
    val poissonDist = new PoissonDistribution(star.i0 / 1000)

    val cutTheta = 0.0 // Currently radial
    val phi = (star.phiMin + star.phiMax) * 0.5 * math.Pi / 180 // Decide how to pick this better

    // for (sim <- simulations; if sim.r0 >= star.rmin && sim.r0 <= star.rmax) {

    //if you only have one folder than you are only looping through one Simulation
    for (sim <- simulations) {

      val beamSize = 0.01 / sim.r0
      val cutSpread = 0.3 / sim.r0
      val scanLength = (star.rmax - star.rmin) / star.duration / sim.r0 / 1000 // Length in R_0 for a millisecond
      println(scanLength.toString())

      val maxStep = sim.maxFileNum
      var step = sim.maxFileNum

      //Buffer means a sequebce which you can add to the front as well as the end
      val scans = collection.mutable.Buffer[Scan]()
      println(s"step = $step")
      while (scans.length < 1000 && step >= 2000 && new File(
               sim.dir,
               s"CartAndRad.$step.bin"
             ).exists()) {
        try {
          //why do we need to check if dataset contains it? Dataset will never contain it because step gets decreased
          //ASKKK????
          if (!dataSets.contains(sim -> step)) {
            val particles = data.CartAndRad.read(
              new File(sim.dir, "CartAndRad." + sim.maxFileNum + ".bin")
            )
            val p2 =
              if (azimuthal) particles.map(p => p.copy(x = p.y, y = p.x))
              else particles
            dataSets(sim -> step) = (p2 -> binParticles(p2))
            //putting a key of (simulation, step) to a (particles, BinData)
          }

          val (particles, binned) = dataSets(sim -> step)
          println(s"Using step $step with ${particles.length} particles.")
          val (zmin, zmax) = {

            //sort the Particles by z field
            val sorted = particles.map(_.z).sorted
            (sorted(100), sorted(sorted.length - 100))
            //choosing zmin zmax from the IndexedSeq[Particles]
          }
          val (cx, cy) =
            //an optional argument
            if (!centerOnMedians) (0.0, 0.0)
            else {
              particles.sortBy(_.x).apply(particles.length / 2).x -> particles
                .sortBy(_.y)
                .apply(particles.length / 2)
                .y
            }

          //actual occultation I believe
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
                sim.dir,
                "CartAndRad." + sim.maxFileNum + ".bin"
              )
            )
        }
        step -= 1000
      }
      pw.println(star)
      pw.println(sim)

      //what is this?!?!?, uses step + 1000, that is basically max step if it iterates twice
      pw.println(
        "Used orbits " + (step + 1000) / 1000.0 + " to " + maxStep / 1000.0
      )
      pw.println(
        "Index\tPhotons\tTrans\tFraction\tstart-x\tstart-y\tend-x\tend-y"
      )
      for ((scan, i) <- scans.zipWithIndex) {
        pw.println(s"$i\t${scan.photons.length}\t${scan.photons
          .count(!_.hit)}\t${scan.intensity}\t${scan.sx}\t${scan.sy}\t${scan.ex}\t${scan.ey}")
      }
      pw.flush()
    }
    // }
  }
}
