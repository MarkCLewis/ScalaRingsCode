package occultations

import java.io.File
import org.apache.commons.math3.distribution.PoissonDistribution
import SynthOccultations._
import util.Particle
import java.io.PrintWriter

object AutomateOccultations extends App {
  case class MeasurementDetails(
    star: String,
    date: String,
    B: Double, // Angle between ring plane and line if sight to the star, degrees 
    phiMin: Double, // Angle between radial direction and line of sight dropped to the ring plane, counterclockwise, degrees
    phiMax: Double,
    rmin: Double, // km
    rmax: Double, // km
    duration: Double, // sec
    i0: Double // Hz
    )

  val stars = Vector(
    MeasurementDetails("α Vir (8) I", "2005-141", 17.2, 116.1, 150.2, 118979, 141954, 2546, 479000),
    MeasurementDetails("α Vir (8) E", "2005–141", 17.2, 116.1, 82.2, 118979, 141704, 2535, 509000),
    MeasurementDetails("α Vir (30) I", "2006–285", 17.2, 266.2, 219.8, 64022, 151545, 4772, 535000),
    MeasurementDetails("α Vir (34) I", "2006–337", 17.2, 282.1, 220.9, 74536, 153654, 4061, 506000),
    MeasurementDetails("α Vir (34) E", "2006–337", 17.2, 282.1, 344.6, 74536, 160059, 4281, 516000),
    MeasurementDetails("α Vir (116) I", "2009–223", 17.2, 245.2, 241.9, 103059, 144566, 3764, 165000),
    MeasurementDetails(" Vir (124) E", "2010–011", 17.2, 121.7, 124.1, 70494, 142431, 6011, 165000),
    MeasurementDetails("α Leo (9) I", "2005–159", 9.5, 68.0, 10.7, 114150, 204718, 6948, 46500),
    MeasurementDetails("α Leo (9) E", "2005–159", 9.5, 68.0, 98.4, 114150, 131539, 2663, 43200),
    MeasurementDetails("γ Peg (36) I", "2006–363", 20.3, 101.6, 156.6, 102296, 178178, 9939, 73000),
    MeasurementDetails("γ Peg (36) E", "2006–363", 20.3, 101.6, 55.7, 102296, 146785, 7172, 70100))

  case class Simulation(
    dir: File,
    maxFileNum: Int,
    r0: Double,
    q: Double,
    radMin: Double,
    radMax: Double,
    rho: Double,
    sigma: Double)

  val simDataDirectory = "/data/mlewis/Rings/JoshCDAP/"
  val DirRegex = """a=(\d+):q=(.+):min=(.+):max=(.+):rho=(.+):sigma=(\d+)""".r
  val FileRegex = """CartAndRad\.(\d+)\.bin""".r

  val simulations = for (dirStr @ DirRegex(r0Str, qStr, minStr, maxStr, rhoStr, sigmaStr) <- new File(simDataDirectory).list) yield {
    val dir = new File(simDataDirectory, dirStr)
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
    num.map(n => Simulation(dir, n, r0, q, radMin, radMax, rho, sigma))
  }

  // 1000 measurements per second.
  // Calculate variance for 1000 measurements
  // Check both uniform and non-uniform photon distributions
  // Use number of photons with a mean of i0/1000 taken from a Poisson distribution

  val dataSets = collection.mutable.Map[(Simulation, Int), (IndexedSeq[Particle], BinData)]()
  
  val pw = new PrintWriter("occultations.txt")

  for (star <- stars) {
    val poissonDist = new PoissonDistribution(star.i0 / 1000)
    val cutTheta = 0.0 // Currently radial
    val phi = (star.phiMin + star.phiMax) * 0.5 * math.Pi/180 // Decide how to pick this better
    for (Some(sim) <- simulations; if sim.r0 >= star.rmin && sim.r0 <= star.rmax) {
      val beamSize = 0.01 / sim.r0
      val cutSpread = 0.3 / sim.r0
      val scanLength = (star.rmax - star.rmin) / star.duration / sim.r0 / 1000 // Length in R_0 for a millisecond
      val maxStep = sim.maxFileNum 
      var step = sim.maxFileNum
      val scans = collection.mutable.Buffer[Scan]()
      while (scans.length < 1000 && step >= 2000 && new File(sim.dir, "CartAndRad." + sim.maxFileNum + ".bin").exists()) {
        if (!dataSets.contains(sim -> step)) {
          val particles = data.CartAndRad.read(new File(sim.dir, "CartAndRad." + sim.maxFileNum + ".bin"))
          dataSets(sim -> step) = (particles -> binParticles(particles))
        }
        val (particles, binned) = dataSets(sim -> step)
     		println(s"Using step $step with ${particles.length} particles.")
        val (zmin, zmax) = {
          val sorted = particles.map(_.z).sorted
          (sorted(100), sorted(sorted.length - 100))
        }
        scans ++= multipleCuts(0, 0, phi, star.B * math.Pi/180, cutTheta, scanLength,
          0.0, beamSize, zmax - zmin, binned, poissonDist.sample, cutSpread).flatten
        println("Scans length = "+scans.length)
        step -= 1000
      }
      pw.println(star)
      pw.println(sim)
      pw.println("Used orbits "+(step+1000)/1000.0+" to "+maxStep/1000.0)
      pw.println("Index\tPhotons\tTrans\tFraction")
      for ((scan, i) <- scans.zipWithIndex) {
        pw.println(i + "\t" + scan.photons.length + "\t" + scan.photons.count(!_.hit) + "\t" + scan.intensity)
      }
    }
  }
}