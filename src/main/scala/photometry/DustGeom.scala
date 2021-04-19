package photometry

import swiftvis2.raytrace._
import data.StepCollData
import data.CollisionData
import swiftvis2.plotting.ColorGradient

case class DustGeom(center: Point, axis1: Vect, axis2: Vect, axis3: Vect, density: Double,color: (Point) => RTColor) 
    extends ScatterGeometry {

  val r1 = axis1.magnitude
  val r2 = axis2.magnitude
  val r3 = axis3.magnitude
  val a1 = axis1.normalize
  val a2 = axis2.normalize
  val a3 = axis3.normalize
  val mag = r1 * r1 + r2 * r2 + r3 * r3

  override val boundingSphere: Sphere = new BoundingSphere(center, r1 max r2 max r3)

  // This is overly conservative and assumes a sphere with a radius of the largest axis
  override def boundingBox: Box = {
    val radius = r1 max r2 max r3
    BoundingBox(center - radius, center + radius)
  }
 
  //vector math here? Cross and dot?
  override def intersect(r: swiftvis2.raytrace.Ray): Option[swiftvis2.raytrace.IntersectData] = {
    // val ellipIdOption = super.intersect(r)
    // ellipIdOption.flatMap { ellipId =>
    //   val penetration = math.random()
    //   if (penetration > density) None else {
    //     val time = ellipId.time + penetration * axis1.magnitude / r.dir.magnitude
    //     val point = r.point(time)
    //     val norm = Vect(math.random(), math.random(), math.random()).normalize
    //     val color = RTColor.White
    //     val reflect = 0.0
    //     val geom = this
    //     Some(IntersectData(time, point, norm, color, reflect, geom))
    //   }
    // }
        
    val ray = r.dir
    val r0 = (r.p0 - center)
    val a = (a1 dot ray) * (a1 dot ray) / (r1 * r1) + (a2 dot ray) * (a2 dot ray) / (r2 * r2) + (a3 dot ray) * (a3 dot ray) / (r3 * r3)
    val b = 2 * ((a1 dot r0) * (a1 dot ray) / (r1 * r1) + (a2 dot r0) * (a2 dot ray) / (r2 * r2) + (a3 dot r0) * (a3 dot ray) / (r3 * r3))
    val c = (a1 dot r0) * (a1 dot r0) / (r1 * r1) + (a2 dot r0) * (a2 dot r0) / (r2 * r2) + (a3 dot r0) * (a3 dot r0) / (r3 * r3) - 1
    val root = b * b - 4 * a * c
    
    
    if (root < 0) None
    else {
      val tmin = (-b - Math.sqrt(root)) / (2 * a)
      val tmax = (-b + Math.sqrt(root)) / (2 * a)
      
      if (tmax < 0) None
      else {      
        val penetration = math.random() // penetration is a fraction of the way between s1 and t2
        val tsep = tmax - tmin
        val crossLength = r.dir.magnitude * tsep
        val crossDepth = crossLength * density
        val intersectProb = 1.0 - math.exp(-crossDepth)
        
        if (penetration > intersectProb) None 
        else {
          val pointInCloud  = (penetration - intersectProb)/(1 - intersectProb)
          //println(crossLength, crossDepth, intersectProb, penetration, pointInCloud)
          val t = (tsep * pointInCloud) + tmin

          val reflect = 0.0
          val pnt = r point t // somewhere between t1 and s2, t1 and s2 are the edges of the ellipsoid?? / when the ray passes through the ellipsoid?
          // val separation = (pnt - center).normalize
          val normal = Vect(math.random(), math.random(), math.random()).normalize
          // val normal = a1 * (a1 dot separation) * mag / r1 + a2 * (a2 dot separation) * mag / r2 + a3 * (a3 dot separation) * mag / r3
          Some(new IntersectData(t, pnt, normal, color(pnt), reflect, this))
        }
      }
    }
  }

  def fractionScattered(incomingDir: Vect, outgoingDir: Vect, intersectData: IntersectData): Double = {
    1.0
  }
}

object DustGeom {
  val dustGrad = ColorGradient(0.0 -> 0xffff0000, 2.0 -> 0xff0000ff)
  def buildFromCollisions(collisions: IndexedSeq[StepCollData], size: Double, renderStep: Int, stepsToMaintain: Int, dustScale: Double): Geometry = {
    val geom = new OctreeScene(Point(0, 0, 0), size)
    for {scd <- collisions
      if scd.step > renderStep - stepsToMaintain && scd.step < renderStep
      coll <- scd.colls
    } {
      val dust = dustFromCollision(coll, dustScale, renderStep - scd.step)
      // dust.foreach(println)
      dust.foreach(geom.addGeom)
    }
    geom
  }

  def dustFromCollision(coll: CollisionData, dustScale: Double, stepsSinceCollision: Int): Seq[Geometry] = {
    // Consider 5x impact speed for width
    // Off dimension "breaths"
    // determine grazing collisions they give multiple exterior clouds.
    val width = (coll.vel max 1e-6) * 5
    val drift = width * 1.5*stepsSinceCollision/1000.0 + 1e-7
    val yDrift = -(coll.p1.x + coll.p2.x)*0.5 * 1.5*stepsSinceCollision/1000.0
    val center = Point((coll.p1.x + coll.p2.x)*0.5, (coll.p1.y + coll.p2.y)*0.5 + yDrift, (coll.p1.y + coll.p2.y)*0.5)
    val axis1 = Vect(width, -drift, 0)
    val axis2 = Vect(drift, width, 0.0) / math.sqrt(drift*drift + width*width) * 1e-7
    val axis3 = Vect(0, 0, 1e-8)
    val spreadFactor = 1.5*stepsSinceCollision/1000.0
    List(DustGeom(center, axis1, axis2, axis3, dustScale/*spreadFactor*/, _ => RTColor(dustGrad(stepsSinceCollision/1000.0))))
    // List(GeomSphere(center, 1e-6, _ => RTColor.Red, _ => 0.0))
  }
}

/*
// Given error rates series
// period P - index of a sample
// S - length of an interval containing p
// R - threshold
// Is there an interval of length S containing P such that all values inside are at least R

def errorWindow(rates: Array[Int], p: Int, s: Int, r: Int): Boolean = {
  var i = p-s max 0
  var start = -1
  while (i < p+s && i < rates.length) {
    if (rates(i) >= r) {
      if (start < 0) start = i
    } else {
      if (start >= 0) {
        if (i - start >= s) return true
        start = -1
      }
    }
    i += 1
  }
  if (start < 0) false
  else i - start >= s
}
                                     s           i             
val errors = Array(5, 10, 7, 15, 0, 60, 55, 60, 60, 60, 0, 5, 5)
errorWindow(errors, 7, 4, 12) == true
errorWindow(errors, 3, 4, 12) == false

t - weighted threshold s*r >= t, r is minimum rate in that window

// Start at p-1, p+1
def errorWindow(rates: Array[Int], p: Int, t: Int): Boolean = {
  for (start <- p to 0 by -1; end <- p until rates.length) {
    val min = (start to end).map(rates).min
    if ((end - start + 1) * min >= t) return true
  }
  false
}
*/