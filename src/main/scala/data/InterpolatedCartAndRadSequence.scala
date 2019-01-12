package data

import util.Particle

/**
 * This class is intended to give sequences of particles that are interpolated between consecutive steps in data files.
 * Note that it is not thread-safe. To help with performance and not use too much memory, this class keeps track of two
 * sets of particles from the two consecutive steps around the last request. When a new request is made, it will try to
 * reuse those if the new request is at a time adjacent to either.
 */
class InterpolatedCartAndRadSequence(dir: java.io.File, startIndex: Int, endIndex: Int, interpCutoff: Double) {
  private val steps = (startIndex to endIndex).filter { i => new java.io.File(dir, "CartAndRad." + i + ".bin").exists() }
  private val interpData = Array.tabulate(2)(i => i -> CartAndRad.read(file(i)))

  def particlesAtTime(time: Double): Seq[Particle] = {
    val modTime = time % (endIndex - startIndex)
    val Some(index) = steps.indices.find(i => steps(i + 1) - startIndex > modTime)
    if (index != interpData(0)._1) {
      for (i <- interpData.indices) interpData(i) = interpData.find(_._1 == index + i).getOrElse((index + i) -> CartAndRad.read(file(index + i)))
    }
    val t1 = steps(index) - startIndex
    val t2 = steps(index + 1) - startIndex
    println(s"dir = $dir, startIndex = $startIndex, time = $time, modTime = $modTime, index = $index, t1 = &t1, t2 = $t2")
    if (t1 == modTime) interpData(0)._2
    else if (t2 == modTime) interpData(1)._2
    else {
      val frac = (modTime - t1) / (t2 - t1)
      println(s"frac = $frac")
      (for {
        i <- interpData(0)._2.indices.par
        p1 = interpData(0)._2(i)
        p2 = interpData(1)._2(i)
        if (p1 distance p2) < interpCutoff
      } yield {
        val px = p1.x + frac * (p2.x - p1.x)
        val py = p1.y + frac * (p2.y - p1.y)
        val pz = p1.z + frac * (p2.z - p1.z)
        val pvx = p1.vx + frac * (p2.vx - p1.vx)
        val pvy = p1.vy + frac * (p2.vy - p1.vy)
        val pvz = p1.vz + frac * (p2.vz - p1.vz)
        Particle(px, py, pz, pvx, pvy, pvz, p1.rad)
      }).seq
    }
  }

  private def file(i: Int): java.io.File = new java.io.File(dir, "CartAndRad." + steps(i) + ".bin")
}