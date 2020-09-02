package photometry

import swiftvis2.raytrace._

// Draw stuff using photometry
object Render {
  def main(args: Array[String]): Unit = {
    
  }

  def render(
      geom: ScatterGeometry,
      light: PointLight,
      viewLoc: Point,
      forward: Vect, // Assume unit vector
      up: Vect, // Assume unit vector
      image: RTImage,
      numPhotons: Long
  ): Unit = {
    val interRect = geom.boundingBox
    val (xmin, xmax, ymin, ymax) = (interRect.min.x, interRect.max.x, interRect.min.y, interRect.max.y)
    val right = forward.cross(up)
    val pixels = Array.fill(image.width, image.height)(RTColor.Black)
    for(_ <- 0L until numPhotons) {
      val ray = Ray(light.point, Point(xmin+math.random()*(xmax-xmin), ymin+math.random()*(ymax-ymin), 0.0)) // Goes to random point in the plane
      val iDataOpt = geom.intersect(ray)
      iDataOpt.foreach { iData =>
        val inRay = (viewLoc-iData.point).normalize
        val scatter = geom.fractionScattered(ray.dir, inRay, iData)
        val px = ((-inRay.dot(right)/0.707 + 1.0)*image.width/2).toInt
        val py = ((inRay.dot(up)/0.707 + 1.0)*image.height/2).toInt
        if (px >= 0 && px < image.width && py >= 0 && py < image.height) {
          pixels(px)(py) += RTColor.White * scatter
        }
      }
    }
    for (px <- 0 until image.width; py <- 0 until image.height) {
      image.setColor(px, py, pixels(px)(py))
    }
  }
}
