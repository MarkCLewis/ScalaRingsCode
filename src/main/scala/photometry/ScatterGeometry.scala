package photometry

import swiftvis2.raytrace._

trait ScatterGeometry extends Geometry {
  def fractionScattered(incomingDir: Vect, outgoingDir: Vect, intersectData: IntersectData): Double
}
