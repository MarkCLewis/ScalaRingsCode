package photometry

import swiftvis2.raytrace._

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
