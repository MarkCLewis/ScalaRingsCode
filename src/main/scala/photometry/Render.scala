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
import data.HighVelocityCollisions

// Draw stuff using photometry
object Render {
  def main(args: Array[String]): Unit = {
    val step = 10000
//    val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/HighRes/Moonlet4d/CartAndRad." + step.toString + ".bin")
    val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/MesoScaleFeatures/AGUPosterRun/a=123220:q=2.8:min=1.5e-9:max=1.5e-8:rho=0.4:sigma=45.5/CartAndRad.4420.bin")
//    val impactURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/HighRes/Moonlet4d/HighVelColls.bin")
    val ringGeom = new KDTreeGeometry[BoundingBox](data.CartAndRad.readStream(carURL.openStream)
      .filter(p => p.y < 2e-5 && p.y > -2e-5)
      .map(p => new ScatterSphereGeom(Point(p.x, p.y, p.z), p.rad, _ => new RTColor(1, 1, 1, 1), _ => 0.0)), 5, BoxBoundsBuilder)
  //  val impacts = HighVelocityCollisions.readStream(impactURL.openStream()).takeRight(100)
  //  println(impacts.last)
  //  println(impacts.foldLeft(0)(_ + _.colls.length))
  //  val impactGeom = new KDTreeGeometry[BoundingSphere](impacts.flatMap(scd => scd.colls
    //   .map(coll => new ScatterSphereGeom(Point(coll.p1.x, coll.p1.y, coll.p1.z), coll.p1.rad*100 min 1e-7, 
    //   _ => new RTColor(coll.vel/5e-6 min 1.0, 0, 1.0 - coll.vel/5e-6 min 1.0, 1), _ => 0.0))))
    // val dustGeom = new DustGeom(Point(0,0,0), Vect(5e-6, 0, 0), Vect(0, 2e-5, 0), Vect(0, 0, 1e-7), 0.5/1e-6)
    val geom = new ListScene(ringGeom) //, dustGeom, impactGeom)
    val lights = List(PhotonSource(PointLight(RTColor(1, 1, 1), Point(1, 0, 0.2), Set.empty), 100000), PhotonSource(PointLight(new RTColor(1.0, 0.8, 0.2), Point(-1e-1, 0, 1e-2)), 20000))
    val viewLoc = Point(0, 0, 7e-6)
    val forward = Vect(0, 0, 1)
    val up = Vect(0, 1, 0)
    val bimg = new BufferedImage(1200, 1200, BufferedImage.TYPE_INT_ARGB)
    val img = new rendersim.RTBufferedImage(bimg)
    val threads: Int = 24

    val frame = new MainFrame {
      title = "Dust Frame"
      contents = new Label("", Swing.Icon(bimg), Alignment.Center)
    }
    frame.visible = true

    def parallelRender(startPixels: Array[Array[RTColor]], pass: Int): Future[Array[Array[RTColor]]] = {
      println(pass)
      val futures = for(c <- 1 to threads; light <- lights) yield Future {
        render(geom, light, viewLoc, forward, up, img)
      }
      Future.sequence(futures).map { pixelFrames =>
        val endPixels = pixelFrames.foldLeft(startPixels)(addPixels)
        writeToImage(endPixels, img)
        frame.repaint()
        endPixels
      }.flatMap(ep => parallelRender(ep, pass + 1))
    }

    parallelRender(Array.fill(img.width, img.height)(RTColor.Black), 0)
  }

  def render(
      geom: Geometry,
      source: PhotonSource,
      viewLoc: Point,
      forward: Vect, // Assume unit vector
      up: Vect, // Assume unit vector
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
      image.setColor(px, py, (pixels(px)(py) / maxPix).copy(a = 1.0))
    }
  }
}
