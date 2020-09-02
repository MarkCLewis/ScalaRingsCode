package photometry

import swiftvis2.raytrace._

class DustGeom extends ScatterGeometry {
  //input points + radius for bounding box & sphere
  //add a light source and/or photons to know incomingDir 
  def boundingBox: swiftvis2.raytrace.Box = ???
  def boundingSphere: swiftvis2.raytrace.Sphere = ???

  //vector math here? Cross and dot?
  def intersect(r: swiftvis2.raytrace.Ray): Option[swiftvis2.raytrace.IntersectData] = ???

  //intersectData producing outgoingDir? If so why is outgoingDir an arguement?
  def fractionScattered(incomingDir: Vect, outgoingDir: Vect, intersectData: IntersectData): Double = {
    1.0
  }
}
