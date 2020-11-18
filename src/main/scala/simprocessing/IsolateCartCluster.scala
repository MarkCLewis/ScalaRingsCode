package simprocessing
import data.CartAndRad
import util.Particle

object IsolateCartCluster{
  def main(args: Array[String]): Unit = {
    val inFile = args(0)
    val outFile = args(1)
    val particles = CartAndRad.read(new java.io.File(inFile))
    val bigBoy = particles.maxBy(_.rad)
    val survivors = particles.filter(p => distance(p, bigBoy) < 1.5*bigBoy.rad)
    CartAndRad.write(new java.io.File(outFile), survivors)
  }
  def distance(part1:Particle, part2:Particle): Double = {
    val dx = part1.x - part2.x
    val dy = part1.y - part2.y
    val dz = part1.z - part2.z
    math.sqrt(dx*dx + dy*dy + dz*dz)
  }
}