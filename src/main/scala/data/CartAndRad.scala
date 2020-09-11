package data

import java.io.DataInputStream
import java.io.File
import java.io.FileInputStream
import scala.Vector
import util.Particle
import java.io.BufferedInputStream
import LittleEndianReads._
import java.io.InputStream
import scalafx.scene.input.KeyCode.P
import java.io.DataOutputStream
import java.io.OutputStream
import java.io.FileOutputStream

object CartAndRad {
  def read(file:File, ymin:Double = Double.MinValue, ymax:Double = Double.MaxValue):IndexedSeq[Particle] = {
    readStream(new FileInputStream(file), ymin, ymax)
  }

  def write(file: File, ps: IndexedSeq[Particle]): Unit = {
    writeStream(new FileOutputStream(file), ps)
  }

  def readStream(is: InputStream, ymin:Double = Double.MinValue, ymax:Double = Double.MaxValue):IndexedSeq[Particle] = {
    val dis = new DataInputStream(new BufferedInputStream(is))
    val num = readInt(dis)
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
    dis.close
    parts
  }

  def writeStream(os: OutputStream, ps: IndexedSeq[Particle]): Unit = {
    val dos = new DataOutputStream(os)
    LittleEndianWrites.writeInt(dos, ps.length)
    for (p <- ps) writeCart(dos,p)
    for (p <- ps) LittleEndianWrites.writeDouble(dos, p.rad)
    dos.close
  }
  
  
  private def readCart(dis:DataInputStream, i:Double):Particle = {
    Particle(readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis),readDouble(dis), i)
  }

  private def writeCart(dos: DataOutputStream, p: Particle): Unit = {
    LittleEndianWrites.writeDouble(dos, p.x)
    LittleEndianWrites.writeDouble(dos, p.y)
    LittleEndianWrites.writeDouble(dos, p.z)
    LittleEndianWrites.writeDouble(dos, p.vx)
    LittleEndianWrites.writeDouble(dos, p.vy)
    LittleEndianWrites.writeDouble(dos, p.vz)
  }

  val FileRegex = """.*CartAndRad.(\d+).bin""".r
  def findAllInDir(dir: File): Array[(String, Int)] = {
    for (fname@FileRegex(step) <- dir.list()) yield (fname, step.toInt)
  }

//  val data = read(new File("/home/mlewis/Rings/JoshCDAP/a=100000:q=2.8:min=2e-9:max=2e-8:rho=0.7:sigma=200/CartAndRad.1020.bin"))
//  println(data(0))
//  println(data(1))
//  println(data(2))
}