package simprocessing

import data.CartAndRad
import util.Particle
import collection.mutable

object IsolateCartCluster{
  def main(args: Array[String]): Unit = {
    if (args.length < 4) {
      println("Usage: infile outfile (-dist mult | -iter mult)")
      return
    }
    val inFile = args(0)
    val outFile = args(1)
    val mult = args(3).toDouble
    val particles = CartAndRad.read(new java.io.File(inFile))
    val bigBoy = particles.maxBy(_.rad)
    val survivors = if (args(2) == "-dist") {
      particles.filter(p => distance(p, bigBoy) < mult*bigBoy.rad)
    } else {
      val keep = mutable.Buffer[Particle](bigBoy)
      val rest = mutable.Buffer[Particle](particles.filter(_ != bigBoy):_*)
      var done = false
      while (!done) {
        val close = mutable.Buffer[Particle]()
        for (r <- rest; k <- keep) {
          if (distance(r, k) < mult * (r.rad + k.rad)) close += r
        }
        rest --= close
        keep ++= close
        done = close.isEmpty
      }
      keep.toIndexedSeq
    }
    CartAndRad.write(new java.io.File(outFile), survivors)
  }
  def distance(part1:Particle, part2:Particle): Double = {
    val dx = part1.x - part2.x
    val dy = part1.y - part2.y
    val dz = part1.z - part2.z
    math.sqrt(dx*dx + dy*dy + dz*dz)
  }
}