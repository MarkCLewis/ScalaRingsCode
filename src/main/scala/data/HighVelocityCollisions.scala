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
  def read(file: File, startStep: Int = 0, endStep: Int = Int.MaxValue): IndexedSeq[StepCollData] = {
    val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(file)))
    var steps = List.empty[StepCollData]
    var step = 0
    var buffer = new Array[Byte](0)
    while (dis.available() > 0 && step < endStep) {
      val num = readInt(dis)
      if(step % 1000 == 0) println(step, num)
      if(step < startStep) {
        if(buffer.length != num*8*15) buffer = new Array[Byte](num*8*15)
        dis.read(buffer)
      } else {
        val collData = for (_ <- 1 to num) yield {
          val vel = readDouble(dis)
          CollisionData(
            vel,
            Particle(readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis)),
            Particle(readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis), readDouble(dis)))
        }
        steps ::= StepCollData(step, collData)
      }
      step += 1
    }
    dis.close
    steps.reverse.toIndexedSeq
  }
}