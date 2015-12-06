package data

import java.io.DataInputStream
import java.io.File
import java.io.FileInputStream
import scala.Vector
import util.Particle
import java.io.BufferedInputStream

object CartAndRad {
  def read(file:File, ymin:Double = Double.MinValue, ymax:Double = Double.MaxValue):IndexedSeq[Particle] = {
    val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(file)))
    val num = readInt(dis)
//    val cartOnly = Array.fill(num)(readCart(dis))
//    cartOnly.map(_.copy(rad = readDouble(dis))).filter(p => p.y>ymin && p.y<ymax)
    val parts = (0 until num).view.map(i => readCart(dis, i)).filter(p => p.y>ymin && p.y<ymax).force.toArray
    var j = 0
    for(i <- parts.indices) yield {
      while(j < parts(i).rad) {
        readDouble(dis)  // Skip particle with lower indices
        j += 1
      }
      parts(i) = parts(i).copy(rad = readDouble(dis))
      j += 1
    }
    parts
  }
  
  private def readInt(dis:DataInputStream) = Integer.reverseBytes(dis.readInt())
  
  private def readDouble(dis:DataInputStream) = java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes(dis.readLong))
  
  private def readCart(dis:DataInputStream, i:Double):Particle = {
    Particle(readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis), i)
  }

//  val data = read(new File("/home/mlewis/Rings/JoshCDAP/a=100000:q=2.8:min=2e-9:max=2e-8:rho=0.7:sigma=200/CartAndRad.1020.bin"))
//  println(data(0))
//  println(data(1))
//  println(data(2))
}