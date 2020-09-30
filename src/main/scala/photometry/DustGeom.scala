package photometry

import swiftvis2.raytrace._

case class DustGeom(center: Point, axis1: Vect, axis2: Vect, axis3: Vect, density: Double) 
    extends GeomEllipsoid(center, axis1, axis2, axis3, _ => RTColor.White, _ => 0.0) 
    with ScatterGeometry {
 
  //vector math here? Cross and dot?
  override def intersect(r: swiftvis2.raytrace.Ray): Option[swiftvis2.raytrace.IntersectData] = {
    val ellipIdOption = super.intersect(r)
    ellipIdOption.flatMap { ellipId =>
      val penetration = math.random() / density
      if (penetration > 1.0) None else {
        val time = ellipId.time + penetration * axis1.magnitude / r.dir.magnitude
        val point = r.point(time)
        val norm = Vect(math.random(), math.random(), math.random()).normalize
        val color = RTColor.White
        val reflect = 0.0
        val geom = this
        Some(IntersectData(time, point, norm, color, reflect, geom))
      }
    }
  }


  def fractionScattered(incomingDir: Vect, outgoingDir: Vect, intersectData: IntersectData): Double = {
    1.0
  }
}
