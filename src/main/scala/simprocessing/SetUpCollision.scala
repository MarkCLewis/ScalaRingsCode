package simprocessing

import data.CartAndRad
import util.Particle

object SetUpCollision{
  def main(args: Array[String]): Unit = {
    if (args.length < 8) {
      println("Usage: infile outfile x y z vx vy vz")
      return
    }
    val inFile = args(0)
    val outFile = args(1)
    val dx = args(2).toDouble
    val dy = args(3).toDouble
    val dz = args(4).toDouble
    val dvx = args(5).toDouble
    val dvy = args(6).toDouble
    val dvz = args(7).toDouble
    val particles = CartAndRad.read(new java.io.File(inFile))
    val bigBoy = particles.maxBy(_.rad)
    val centeredParticles = particles.map { case Particle(x, y, z, vx, vy, vz, r) => 
      Particle(x - bigBoy.x, y - bigBoy.y, z - bigBoy.z, vx - bigBoy.vx, vy - bigBoy.vy, vz - bigBoy.vz, r)}
    val cluster1 = centeredParticles.map { case Particle(x, y, z, vx, vy, vz, r) =>
      Particle(x + dx, y + dy, z + dz, vx + dvx, vy + dvy, vz + dvz, r)}
    val cluster2 = centeredParticles.map { case Particle(x, y, z, vx, vy, vz, r) =>
      Particle(x - dx, y - dy, z - dz, vx - dvx, vy - dvy, vz - dvz, r)}
    CartAndRad.write(new java.io.File(outFile), cluster1 ++ cluster2)
  }
}

//takes infile outfile -- done
//takes 6 numbers for location and velocity offsets (x, y, z, vx, vy, vz) -- done 
//the  antiBigBoy should be -(x, y, z, vx, vy, vz)
//make it so that it takes in cardAndRad and duplicate cluster, offset both, (centered around 0) velocities should move towards each other