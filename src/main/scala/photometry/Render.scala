package photometry

import swiftvis2.raytrace._
import java.awt.image.BufferedImage
//import scala.swing._
import java.io.File
//import ExtendedSlidingBoxSims.SimSpec
import javax.imageio._
import scala.swing.{MainFrame, Label, Swing, Alignment}


// Draw stuff using photometry
object Render {
  def main(args: Array[String]): Unit = {
    RTColor(255, 255, 255)
    val geom = new ScatterSphereGeom(
      Point(20, 0, 0),
      10,
      Point => RTColor(255, 255, 255, 1),
      Point => 20.0
    )
    val light = PointLight(RTColor(0.1, 0.1, 0.1), Point(0, 0, -20), Set.empty)
    val viewLoc = Point(0, 0, 0)
    val forward = Vect(1, 0, 0)
    val up = Vect(0, 1, 0)
    val bimg = new BufferedImage(1200, 1200, BufferedImage.TYPE_INT_ARGB)
    val img = new rendersim.RTBufferedImage(bimg)

    render(geom, light, viewLoc, forward, up, img, 100000)
    val frame = new MainFrame {
      title = "Dust Frame"
      contents = new Label("", Swing.Icon(bimg), Alignment.Center)
    }
    frame.visible = true
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
    val (xmin, xmax, ymin, ymax) =
      (interRect.min.x, interRect.max.x, interRect.min.y, interRect.max.y)
    val right = forward.cross(up)
    val pixels = Array.fill(image.width, image.height)(RTColor.Black)
    for (_ <- 0L until numPhotons) {
      val ray = Ray(
        light.point,
        Point(
          xmin + math.random() * (xmax - xmin),
          ymin + math.random() * (ymax - ymin),
          0.0
        )
      ) // Goes to random point in the plane
      val iDataOpt = geom.intersect(ray)
      iDataOpt.foreach { iData =>
        val inRay = (viewLoc - iData.point).normalize
        val scatter = geom.fractionScattered(ray.dir, inRay, iData)
        val px = ((-inRay.dot(right) / 0.707 + 1.0) * image.width / 2).toInt
        val py = ((inRay.dot(up) / 0.707 + 1.0) * image.height / 2).toInt
        if (px >= 0 && px < image.width && py >= 0 && py < image.height) {
          pixels(px)(py) += light.col * scatter
        }
      }
    }
    //val maxPix = pixels.foldLeft(0.0)((m,p) => m max p.max)
    for (px <- 0 until image.width; py <- 0 until image.height) {
      image.setColor(px, py, pixels(px)(py))
    }
  }
}
