package occultations

import java.io.File
import scala.collection.mutable
import scala.math.cos
import scala.math.sin
import data.CartAndRad
import util.Particle
import util.Ray
import util.Vect3D
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.PrintWriter

object SynthOccultations {
  case class BinData(xmin: Double, xmax: Double, ymin: Double, ymax: Double, binSize: Double, bins: IndexedSeq[IndexedSeq[IndexedSeq[Particle]]]) {
    lazy val zmin = bins.view.map(r => r.view.map(c => if(c.isEmpty) 1e100 else c.view.map(_.z).min).min).min
    lazy val zmax = bins.view.map(r => r.view.map(c => if(c.isEmpty) -1e100 else c.view.map(_.z).max).max).max
    lazy val maxRadius = bins.view.map(r => r.view.map(c => if(c.isEmpty) -1e100 else c.view.map(_.rad).max).max).max
    def gridXMin(bin:Int) = xmin+bin*binSize
    def gridXMax(bin:Int) = (xmin+1)+bin*binSize
    def gridYMin(bin:Int) = ymin+bin*binSize
    def gridYMax(bin:Int) = (ymin+1)+bin*binSize
  }

  case class Photon(x: Double, y: Double, hit: Boolean)
  case class Scan(sx: Double, sy: Double, ex: Double, ey: Double, intensity: Double, photons: Seq[Photon])

  def multipleCuts(x: Double, y: Double, theta: Double, phi: Double, cutTheta: Double, scanLength: Double,
    offLength: Double, beamSize: Double, height: Double, binData: BinData, photons: => Int, cutSpread: Double): Seq[Seq[Scan]] = {
    var my = y
    while (my - cutSpread > binData.ymin) my -= cutSpread
    val ret = mutable.ArrayBuffer[Seq[Scan]]()
    while (my < binData.ymax) {
      println("Cut at " + my)
      ret += syntheticOccultation(x, my, theta, phi, cutTheta, scanLength, offLength, beamSize, height, binData, photons)
      my += cutSpread
    }
    ret
  }

  /**
   * @param x The radial coordinate of a point the occultation crosses in the plane
   * @param y The azimuthal coordinate of a point the occultation crosses in the plane
   * @param theta The angle in the plane from the radial axis of the observation
   * @param phi The elevation angle out of the plane of the observation
   * @param cutTheta The angle in the plane from the radial of the motion of the beam in the plane
   * @param scanLength How long each scan covers when the shutter is open
   * @param offLength How long the scan moves when the shutter is closed
   * @param beamSize The radius of the beam in the ring plane
   */
  def syntheticOccultation(x: Double, y: Double, theta: Double, phi: Double, cutTheta: Double, scanLength: Double,
    offLength: Double, beamSize: Double, height: Double, binData: BinData, photonCount: => Int): Seq[Scan] = {
    val rDir = Vect3D(cos(theta) * cos(phi), sin(theta) * cos(phi), sin(phi))
    val dx = math.cos(cutTheta)
    val dy = math.sin(cutTheta)
    val xstart = binData.xmin + rDir.x.abs * height
    val xend = binData.xmax - rDir.x.abs * height
    println(s"$height, $xstart, $xend, $rDir")
    var mx = xstart
    val ret = mutable.Buffer[Scan]()
    while (mx < xend-scanLength) {
      val sx = mx
      val sy = y + (ret.length * scanLength) * dy
      val ex = sx + scanLength * dx
      val ey = sy + scanLength * dy
      val pc = photonCount
      val start = System.nanoTime()
      val photons = (1 to pc).par.map(_ => {
        val t = math.random
        val rx = sx + t * (ex - sx) + math.random * math.random * beamSize
        val ry = sy + t * (ey - sy) + math.random * math.random * beamSize
        if(rx < binData.xmin || rx > binData.xmax) println(s"Oops x! $rx, ${binData.xmin} ${binData.xmax}")
        if(ry < binData.ymin || ry > binData.ymax) println(s"Oops y! $ry, ${binData.ymin} ${binData.ymax}")
        Photon(rx, ry, rayGridIntersect(Ray(Vect3D(rx, ry, 0), rDir), binData))
      })
      println(s"Did $photonCount photons in ${(System.nanoTime()-start)*1e-9} seconds")
      ret += Scan(sx, sy, ex, ey, photons.count(p => !p.hit).toDouble / pc, photons.seq)
      mx += (scanLength + offLength) * dx
    }
    ret
  }

  def rayGridIntersect(r: Ray, binData: BinData): Boolean = {
    val tmin = (binData.zmin - r.r0.z) / r.r.z
    val tmax = (binData.zmax - r.r0.z) / r.r.z
    val xmin = r.r0.x + tmin * r.r.x
    val xmax = r.r0.x + tmax * r.r.x
    val ymin = r.r0.y + tmin * r.r.y
    val ymax = r.r0.y + tmax * r.r.y
    val minxbin = (((xmin min xmax) - binData.xmin) / binData.binSize).toInt - 1 max 0
    val maxxbin = (((xmin max xmax) - binData.xmin) / binData.binSize).toInt + 1 min binData.bins.length - 1
    val minybin = (((ymin min ymax) - binData.ymin) / binData.binSize).toInt - 1 max 0
    val maxybin = (((ymin max ymax) - binData.ymin) / binData.binSize).toInt + 1 min binData.bins(0).length - 1
    (minxbin to maxxbin).exists(xbin => {
      (minybin to maxybin).exists(ybin => rayBinIntersect(r, xbin, ybin, binData))
    })
  }

  def rayBinIntersect(r: Ray, xbin: Int, ybin: Int, binData: BinData): Boolean = {
    val xminTime = if(r.r.x == 0.0) -1e100 else (binData.gridXMin(xbin) - r.r0.x) / r.r.x
    val xmaxTime = if(r.r.x == 0.0) 1e100 else (binData.gridXMax(xbin) - r.r0.x) / r.r.x
    val yminTime = if(r.r.y == 0.0) -1e100 else (binData.gridYMin(ybin) - r.r0.y) / r.r.y
    val ymaxTime = if(r.r.y == 0.0) 1e100 else (binData.gridYMax(ybin) - r.r0.y) / r.r.y
    val xEnter = xminTime min xmaxTime
    val yEnter = yminTime min ymaxTime
    val xExit = xminTime max xmaxTime
    val yExit = yminTime max ymaxTime
    if(xEnter < yExit && xEnter < xExit) {
      val enterTime = xEnter max yEnter
      val exitTime = xExit min yExit
      val zEnter = r.r0.z + enterTime * r.r.z
      val zExit = r.r0.z + exitTime * r.r.z
      if(binData.bins(xbin)(ybin).nonEmpty && zEnter < binData.bins(xbin)(ybin).last.z && zExit > binData.bins(xbin)(ybin).head.z) {
        val enterIndex = firstParticleTouchingZ(zEnter, binData.bins(xbin)(ybin), binData.maxRadius)
        val exitIndex = lastParticleTouchingZ(zExit, binData.bins(xbin)(ybin), binData.maxRadius) min binData.bins(xbin)(ybin).length-1
        (enterIndex to exitIndex).exists(i => {
//          println(s"$i, $xbin, $ybin, ${binData.bins(xbin)(ybin).length}")
          rayParticleIntersect(r, binData.bins(xbin)(ybin)(i))
        })
      } else false
    } else false
  }
  
  def binarySearchZ(z:Double, parts:IndexedSeq[Particle]):Int = {
    def helper(start: Int, end: Int): Int = {
      if(end-start < 2) start
      else {
        val mid = (start+end)/2
        if(parts(mid).z == z) mid
        else if(parts(mid).z < z) helper(mid+1, end)
        else helper(start, mid)
      }
    }
    helper(0, parts.length)
  }
  
  def firstParticleTouchingZ(z:Double, parts:IndexedSeq[Particle], maxRadius:Double):Int = {
    var index = binarySearchZ(z, parts)
    var i = index-1
    while(i >= 0 && (parts(i).z-z).abs < maxRadius) {
      if((parts(i).z-z).abs < parts(i).rad) index = i
      i -= 1
    }
    index
  }

  def lastParticleTouchingZ(z:Double, parts:IndexedSeq[Particle], maxRadius:Double):Int = {
    var index = binarySearchZ(z, parts)
    var i = index+1
    while(i < parts.length && (parts(i).z-z).abs < maxRadius) {
      if((parts(i).z-z).abs < parts(i).rad) index = i
      i += 1
    }
    index
  }

  def rayParticleIntersect(r: Ray, part: Particle): Boolean = {
    val p = new Vect3D(part.x, part.y, part.z)
    val d = r.r0 - p
    val a = r.r dot r.r
    val b = 2 * (r.r dot d)
    val c = (d dot d) - part.rad * part.rad
    b * b - 4 * a * c >= 0
  }

  def binParticles(parts: IndexedSeq[Particle]): BinData = {
    val (xmin, xmax, ymin, ymax) = parts.foldLeft(Double.MaxValue, Double.MinValue, Double.MaxValue, Double.MinValue)((acc, p) => {
      (acc._1 min p.x, acc._2 max p.x, acc._3 min p.y, acc._4 max p.y)
    })
    val maxRad = parts.foldLeft(0.0)((rad, p) => rad max p.rad)
    val dx = xmax - xmin
    val dy = ymax - ymin
    val partsPerBin = 10.0
    val binSize = math.sqrt(dx * dy * partsPerBin / parts.length) max maxRad
    println(s"$dx $dy $maxRad $binSize ${math.ceil(dx / binSize)} ${dx * dy * partsPerBin / parts.length} $xmax $xmin")
    val ret = Array.fill(math.ceil(dx / binSize).toInt)(mutable.ArrayBuffer.fill(math.ceil(dy / binSize).toInt)(mutable.ArrayBuffer[Particle]()))
    for (i <- parts.indices) {
      val binx = ((parts(i).x - xmin) / binSize).toInt min ret.length - 1
      val biny = ((parts(i).y - ymin) / binSize).toInt min ret(0).length - 1
      ret(binx)(biny) += parts(i)
    }
    for(i <- ret.indices; j <- ret(i).indices) {
      ret(i)(j) = ret(i)(j).sortBy(_.z)
    }
    BinData(xmin, xmax, ymin, ymax, binSize, ret)
  }

}
