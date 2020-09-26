package photometry

import swiftvis2.raytrace._
import java.awt.image.BufferedImage

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
//import scala.swing._
import java.io.File
//import ExtendedSlidingBoxSims.SimSpec
import javax.imageio._
import scala.swing.{MainFrame, Label, Swing, Alignment}
import java.net.URL


// Draw stuff using photometry
object Render {
  def main(args: Array[String]): Unit = {
    RTColor(255, 255, 255)
    val step = 5000
    val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/Moonlet4/CartAndRad." + step.toString + ".bin")
		val geom = new KDTreeGeometry[BoundingSphere](data.CartAndRad.readStream(carURL.openStream).map(p => new ScatterSphereGeom(Point(p.x, p.y, p.z), p.rad, _ => new RTColor(1, 1, 1, 1), _ => 0.0)))
    val lights:List[PointLight] = List(PointLight(RTColor(1, 1, 1), Point(1, 0, 0.2), Set.empty), PointLight(new RTColor(0.5, 0.4, 0.1, 1), Point(-1e-1, 0, 1e-2)))
    val viewLoc = Point(0, 0, 2e-5)
    val forward = Vect(0, 0, 1)
    val up = Vect(0, 1, 0)
    val bimg = new BufferedImage(1200, 1200, BufferedImage.TYPE_INT_ARGB)
    val img = new rendersim.RTBufferedImage(bimg)
    val threads: Int = 4



    val frame = new MainFrame {
      title = "Dust Frame"
      contents = new Label("", Swing.Icon(bimg), Alignment.Center)
    }
    frame.visible = true

    var totalPixels: Array[Array[RTColor]] = null

    val futures = for(c <- 1 to threads) yield Future{
      for (i <- 1 to 100) {
        println(i)
        for (j <- lights) {
          val pixels = render(geom, j, viewLoc, forward, up, img, 100000)
          if (totalPixels == null) totalPixels = pixels else totalPixels = addPixels(totalPixels, pixels)
          writeToImage(totalPixels, img)
          frame.repaint()
        }
      }
    }
    futures.map(Await.result(_, Duration.Inf))
  }

  def render(
      geom: Geometry,
      light: PointLight,
      viewLoc: Point,
      forward: Vect, // Assume unit vector
      up: Vect, // Assume unit vector
      image: RTImage,
      //pixels: Array[Array[RTColor]],
      numPhotons: Long
  ): Array[Array[RTColor]] = {
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
        val interPoint = iData.point + iData.norm*1e-8
        if (geom.intersect(Ray(interPoint, viewLoc)).isEmpty) {
          val inRay = (viewLoc - interPoint).normalize
          val scatter = iData.geom.asInstanceOf[ScatterGeometry].fractionScattered(ray.dir, inRay, iData)
          if (scatter > 0.0) {
            val fracForward = inRay dot forward
            val px = ((inRay.dot(right)/fracForward / 0.707 + 1.0) * image.width / 2).toInt
            val py = ((inRay.dot(up)/fracForward / 0.707 + 1.0) * image.height / 2).toInt
            if (px >= 0 && px < image.width && py >= 0 && py < image.height) {
              pixels(px)(py) += light.col * scatter
            }
          }
        }
      }
    }
    pixels
  }

  def addPixels(pixels1: Array[Array[RTColor]], pixels2: Array[Array[RTColor]]): Array[Array[RTColor]] = {
    Array.tabulate(pixels1.length, pixels1(0).length)((i, j) => pixels1(i)(j) + pixels2(i)(j))
  }

  def writeToImage(pixels: Array[Array[RTColor]], image: RTImage): Unit = {
    val maxPix = pixels.foldLeft(0.0)((m,row) => m max row.foldLeft(0.0)((m2, p) => m2 max p.r max p.g max p.b))
    for (px <- 0 until image.width; py <- 0 until image.height) {
      image.setColor(px, py, (pixels(px)(py) / maxPix).copy(a = 1.0))
    }
  }
}
