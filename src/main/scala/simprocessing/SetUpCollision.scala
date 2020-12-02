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
    val x = args(2)
    val y = args(3)
    val z = args(4)
    val vx = args(5)
    val vy = args(6)
    val vz = args(7)
    val particles = CartAndRad.read(new java.io.File(inFile))
    val bigBoy = particles.maxBy(_.rad)

  }  
}

//takes infile outfile -- done
//takes 6 numbers for location and velocity offsets (x, y, z, vx, vy, vz) -- done 
//the  antiBigBoy should be -(x, y, z, vx, vy, vz)
//make it so that it takes in cardAndRad and duplicate cluster, offset both, (centered around 0) velocities should move towards each other