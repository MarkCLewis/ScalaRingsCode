package photometry

import swiftvis2.raytrace._
import java.awt.image.BufferedImage

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import data.HighVelocityCollisions
import java.io._
import scala.swing.MainFrame


object Render {
  val viewLoc = Point(0, 0, -3e-5)
  val forward = Vect(0, 0, 1)
  val up = Vect(0, 1, 0)
  
  def parallelRender(startPixels: Array[Array[RTColor]], pass: Int, threads: Int, lights: List[PhotonSource], geom: ListScene, img: rendersim.RTBufferedImage, frame: MainFrame): Future[Array[Array[RTColor]]] = {
        println(pass)
        val futures = for(c <- 1 to threads; light <- lights) yield Future {
          render(geom, light, viewLoc, forward, up, img)
        }
        Future.sequence(futures).map { pixelFrames =>
          val endPixels = pixelFrames.foldLeft(startPixels)(addPixels)
          writeToImage(endPixels, img)
          frame.repaint()
          endPixels
        }.flatMap(ep => parallelRender(ep, pass + 1, threads, lights, geom, img, frame))
      }


  def render(
      geom: Geometry,
      source: PhotonSource,
      viewLoc: Point,
      forward: Vect, //Assume unit vector
      up: Vect, //Assume unit vector
      image: RTImage
  ): Array[Array[RTColor]] = {
    val interRect = geom.boundingBox
    val (xmin, xmax, ymin, ymax) =
      (interRect.min.x, interRect.max.x, interRect.min.y, interRect.max.y)
    val right = forward.cross(up)
    val pixels = Array.fill(image.width, image.height)(RTColor.Black)

    for (_ <- 0L until source.numPhotons) {
      val ray = Ray(
        source.light.point,
        Point(
          xmin + math.random() * (xmax - xmin),
          ymin + math.random() * (ymax - ymin),
          0.0
        )
      ) 

      val iDataOpt = geom.intersect(ray)
      iDataOpt.foreach { iData =>
        val interPoint = iData.point + iData.norm*1e-8
        if (geom.intersect(Ray(interPoint, viewLoc)).isEmpty) {
          val inRay = (viewLoc - interPoint).normalize
          val scatter = iData.geom.asInstanceOf[ScatterGeometry].fractionScattered(ray.dir, inRay, iData)
          if (scatter > 0.0) {
            val fracForward = inRay dot forward
            val px = ((inRay.dot(right)/fracForward / 0.707 + 1.0) * image.width / 2).toInt
            val py = ((-inRay.dot(up)/fracForward / 0.707 + 1.0) * image.height / 2).toInt
            if (px >= 0 && px < image.width && py >= 0 && py < image.height) {
              pixels(px)(py) += source.light.col * iData.color * scatter
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
      image.setColor(px, py, (pixels(px)(py) / maxPix * 2.0).copy(a = 1.0))
    }
  }
}
