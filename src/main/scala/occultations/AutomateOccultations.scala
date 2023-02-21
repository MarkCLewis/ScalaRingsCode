package occultations

import java.io.File
import org.apache.commons.math3.distribution.PoissonDistribution
import SynthOccultations._
import util.Particle
import java.io.PrintWriter
import collection.JavaConverters._

object AutomateOccultations {
  def main(args: Array[String]): Unit = {
    if(args.length < 1) {
      println("Specify the directory above the simulation directory that you want to process.")
      println("Optional Arguments:")
      println(" -azimuthal: transposes x and y in the data so azimuthal cuts are made.")
      println(" -starFile: delimited file of star data.")
      sys.exit(0)
    }
    val (azimuthal, args2) = {
      val (a1, a2) = args.partition(_ == "-azimuthal")
      (a1.nonEmpty, a2)
    }
    val (starData, args3) = {
      val ind = args2.indexOf("-starFile")
      if (ind < 0) (MeasurementDetails.baseStars, args2) else {
        val source = scala.io.Source.fromFile(args2(ind + 1))
        val sd = source.getLines.flatMap(line => MeasurementDetails.fromCSV(line)).toVector
        val sd2 = if (azimuthal) sd.map(md => md.copy(phiMin = 0.0, phiMax = 0.0)) else sd
        source.close()
        (sd2, args2.patch(ind, Nil, 2))
      }
    }
    println(starData)
    val simDataDirectory = args3(0) //"/home/mlewis/Rings/JoshCDAP15-17/"
    val simDataDirectoryFile = new File(simDataDirectory)
    if(!simDataDirectoryFile.exists()) {
      println("You need to specify a directory that exists.")
      sys.exit(0)
    }
    val centerOnMedians = args.contains("-centerOnMedians")
    val DirRegex = """a=([\d.]+):q=([\d.]+):min=([\d.eE-]+):max=([\d.eE-]+):rho=([\d.]+):\w+=([\d.]+)(.*)""".r
    val FileRegex = """CartAndRad\.(\d+)\.bin""".r
    val directories = simDataDirectoryFile.list
    println("Directories = "+directories.mkString(", "))

    val simulations = (for (dirStr @ DirRegex(r0Str, qStr, minStr, maxStr, rhoStr, sigmaStr, rest) <- directories) yield {
      println(dirStr)
      val dir = new File(simDataDirectoryFile, dirStr)
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
      num.map(n => Simulation(dir, n, r0, q, radMin, radMax, rho, sigma, rest))
    }).flatten

    simulations.foreach(println)

    // 1000 measurements per second.
    // Calculate variance for 1000 measurements
    // Check both uniform and non-uniform photon distributions
    // Use number of photons with a mean of i0/1000 taken from a Poisson distribution

    //val dataSets = collection.mutable.Map[(Simulation, Int), (IndexedSeq[Particle], BinData)]()
    val dataSets = new java.util.WeakHashMap[(Simulation, Int), (IndexedSeq[Particle], BinData)]().asScala
    
    val pw = new PrintWriter(new File(simDataDirectoryFile, "occultations.txt"))

    for (star <- starData) {
      val poissonDist = new PoissonDistribution(star.i0 / 1000)
      val cutTheta = 0.0 // Currently radial
      val phi = (star.phiMin + star.phiMax) * 0.5 * math.Pi/180 // Decide how to pick this better
      for (sim <- simulations; if sim.r0 >= star.rmin && sim.r0 <= star.rmax) {
        println(sim)
        val beamSize = 0.01 / sim.r0
        val cutSpread = 0.3 / sim.r0
        val scanLength = (star.rmax - star.rmin) / star.duration / sim.r0 / 1000 // Length in R_0 for a millisecond
        val maxStep = sim.maxFileNum 
        var step = sim.maxFileNum
        val scans = collection.mutable.Buffer[Scan]()
        println(s"step = $step")
        while (scans.length < 1000 && step >= 2000 && new File(sim.dir, s"CartAndRad.$step.bin").exists()) {
          try {
            if (!dataSets.contains(sim -> step)) {
              val particles = data.CartAndRad.read(new File(sim.dir, "CartAndRad." + sim.maxFileNum + ".bin"))
              val p2 = if (azimuthal) particles.map(p => p.copy(x = p.y, y = p.x)) else particles
              dataSets(sim -> step) = (p2 -> binParticles(p2))
            }
            val (particles, binned) = dataSets(sim -> step)
            println(s"Using step $step with ${particles.length} particles.")
            val (zmin, zmax) = {
              val sorted = particles.map(_.z).sorted
              (sorted(100), sorted(sorted.length - 100))
            }
            val (cx, cy) = if (!centerOnMedians) (0.0, 0.0) else {
              particles.sortBy(_.x).apply(particles.length/2).x -> particles.sortBy(_.y).apply(particles.length/2).y
            }
            scans ++= multipleCuts(cx, cy, phi, star.B * math.Pi/180, cutTheta, scanLength,
              0.0, beamSize, zmax - zmin, binned, poissonDist.sample, cutSpread, 2000).flatten
            println("Scans length = "+scans.length)
          } catch {
            case ex:java.io.IOException =>
              println("Problem reading "+new File(sim.dir, "CartAndRad." + sim.maxFileNum + ".bin"))
          }
          step -= 1000
        }
        pw.println(star)
        pw.println(sim)
        pw.println("Used orbits "+(step+1000)/1000.0+" to "+maxStep/1000.0)
        pw.println("Index\tPhotons\tTrans\tFraction\tstart-x\tstart-y\tend-x\tend-y")
        for ((scan, i) <- scans.zipWithIndex) {
          pw.println(s"$i\t${scan.photons.length}\t${scan.photons.count(!_.hit)}\t${scan.intensity}\t${scan.sx}\t${scan.sy}\t${scan.ex}\t${scan.ey}")
        }
        pw.flush()
      }
    }
  }
}
