package photometry

import swiftvis2.raytrace.Vect

class DustGeom extends ScatterGeometry {
  def boundingBox: swiftvis2.raytrace.Box = ???
  def boundingSphere: swiftvis2.raytrace.Sphere = ???
  def intersect(r: swiftvis2.raytrace.Ray): Option[swiftvis2.raytrace.IntersectData] = ???

  def fractionScattered(dir: Vect): Double = {
    // TODO:
    1.0
  }
}
