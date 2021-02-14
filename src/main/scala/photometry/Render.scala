package photometry

import swiftvis2.raytrace._
import java.awt.image.BufferedImage

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import data.HighVelocityCollisions
import java.io._
import scala.swing.MainFrame
import scala.swing.Frame


object Render {  
  
  def parallelRender(vds: Seq[ViewData], currentPass: Int, maxPass: Int, threads: Int, lights: List[PhotonSource], geom: ListScene, frame: Option[Frame]): Future[Seq[ViewData]] = {
    if (currentPass < maxPass) {
      println(currentPass)
      val futures = for(c <- 1 to threads; light <- lights) yield Future {
        render(geom, light, vds)
      }
      Future.sequence(futures).map { pixelFrames =>
        val byView = pixelFrames.transpose
        assert(byView.length == vds.length)
        val endViewDatas = (byView, vds).zipped.map((pixels, vd) => pixels.foldLeft(vd)(_.addPixels(_)))
        endViewDatas.foreach(_.writeToImage())
        frame.foreach((_.repaint()))
        endViewDatas
      }.flatMap(evd => parallelRender(evd, currentPass + 1, maxPass, threads, lights, geom, frame))
    } else Future.successful(vds)
  }

  def render(
      geom: Geometry,
      source: PhotonSource,
      vds: Seq[ViewData]
  ): Seq[Array[Array[RTColor]]] = {
    val interRect = geom.boundingBox
    val (xmin, xmax, ymin, ymax) =
      (interRect.min.x, interRect.max.x, interRect.min.y, interRect.max.y)
    val rights = vds.map(vd => vd.view.dir.cross(vd.view.up))
    val pixelss = vds.map(vd => Array.fill(vd.image.width, vd.image.height)(RTColor.Black))

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
        for ((vd, right, pixels) <- (vds, rights, pixelss).zipped) {
          if (geom.intersect(Ray(interPoint, vd.view.loc)).isEmpty) {
            val inRay = (vd.view.loc - interPoint).normalize
            val scatter = iData.geom.asInstanceOf[ScatterGeometry].fractionScattered(ray.dir, inRay, iData)
            if (scatter > 0.0) {
              val fracForward = inRay dot vd.view.dir
              if (fracForward < 0.0) {
                val px = ((inRay.dot(right)/fracForward / vd.openingAngle + 1.0) * vd.image.width / 2).toInt
                val py = ((-inRay.dot(vd.view.up)/fracForward / vd.openingAngle + 1.0) * vd.image.height / 2).toInt
                if (px >= 0 && px < vd.image.width && py >= 0 && py < vd.image.height) {
                  pixels(px)(py) += source.light.col * iData.color * scatter
                }
              }
            }
          }
        }
      }
    }
    pixelss
  }

}
