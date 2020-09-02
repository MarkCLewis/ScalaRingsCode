package photometry

import swiftvis2.raytrace._

class ScatterSphereGeom(
    center: Point,
    radius: Double,
    color: Point => RTColor,
    reflect: Point => Double
) extends GeomSphere(center, radius, color, reflect)
    with ScatterGeometry {

  def fractionScattered(
      incomingDir: Vect,
      outgoingDir: Vect,
      intersectData: IntersectData
  ): Double = {
    -incomingDir.dot(intersectData.norm)
  }
}

// a dot b = a.x*b.x + a.y*b.y + a.z*b.z = |a|*|b|*cos(theta)