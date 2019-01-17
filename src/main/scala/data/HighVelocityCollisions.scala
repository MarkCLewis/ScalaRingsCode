package data

import util.Particle
import java.io.File
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.DataInputStream
import data.LittleEndianReads._

case class CollisionData(vel: Double, p1: Particle, p2: Particle)
case class StepCollData(step: Int, colls: IndexedSeq[CollisionData])

object HighVelocityCollisions {
  def read(file: File): IndexedSeq[StepCollData] = {
    val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(file)))
    var steps = List.empty[StepCollData]
    var step = 0
    while (dis.available() > 0) {
      val num = readInt(dis)
      val collData = for (_ <- 1 to num) yield {
        val vel = readDouble(dis)
        CollisionData(
          vel,
          Particle(readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis)),
          Particle(readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis)))
      }
      steps ::= StepCollData(step, collData)
      step += 1
    }
    dis.close
    steps.reverse.toIndexedSeq
  }
}