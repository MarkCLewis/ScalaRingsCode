package photometry

import swiftvis2.raytrace.Geometry
import swiftvis2.raytrace.Vect

trait ScatterGeometry extends Geometry {
  def fractionScattered(dir: Vect): Double
}
