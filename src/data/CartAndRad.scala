package data

import java.io.DataInputStream
import java.io.File
import java.io.FileInputStream
import scala.Vector
import util.Particle
import java.io.BufferedInputStream

object CartAndRad {
  def read(file:File):IndexedSeq[Particle] = {
    val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(file)))
    val num = readInt(dis)
    val cartOnly = Array.fill(num)(readCart(dis))
    cartOnly.map(_.copy(rad = readDouble(dis)))
  }
  
  private def readInt(dis:DataInputStream) = Integer.reverseBytes(dis.readInt())
  
  private def readDouble(dis:DataInputStream) = java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes(dis.readLong))
  
  private def readCart(dis:DataInputStream):Particle = {
    Particle(readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis),0.0)
  }

//  val data = read(new File("/home/mlewis/Rings/JoshCDAP/a=100000:q=2.8:min=2e-9:max=2e-8:rho=0.7:sigma=200/CartAndRad.1020.bin"))
//  println(data(0))
//  println(data(1))
//  println(data(2))
}