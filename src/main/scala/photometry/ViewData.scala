package photometry

import swiftvis2.raytrace.Point
import swiftvis2.raytrace.Vect
import swiftvis2.raytrace.RTColor
import scala.swing.Frame
import swiftvis2.raytrace.LinearViewPath.View
import rendersim.RTBufferedImage

// TODO: Change first three to View
case class ViewData(view: View, openingAngle: Double, image: RTBufferedImage, pixels: Array[Array[RTColor]]) {

  def addPixels(pixels1: Array[Array[RTColor]]): ViewData = {
    copy(pixels = Array.tabulate(pixels.length, pixels(0).length)((i, j) => pixels(i)(j) + pixels1(i)(j)))
  }

  def writeToImage(): Unit = {
    val maxPix = pixels.foldLeft(0.0)((m,row) => m max row.foldLeft(0.0)((m2, p) => m2 max p.r max p.g max p.b))
    for (px <- 0 until image.width; py <- 0 until image.height) {
      image.setColor(px, py, (pixels(px)(py) / maxPix * 2.0).copy(a = 1.0))
    }
  }

}

object ViewData {
  val broadAngle = 0.707

  def atOriginFrom(viewLoc: Point, openingAngle: Double, image: RTBufferedImage): ViewData = {
    fromTo(viewLoc, Point(0, 0, 0), openingAngle, image)
  }

  def lookingDown(viewLoc: Point, openingAngle: Double, image: RTBufferedImage): ViewData = {
    val forward = Vect(0, 0, 1)
    val up = Vect(0, 1, 0)
    new ViewData(View(viewLoc, forward, up), openingAngle, image, Array.fill(image.width, image.height)(RTColor.Black))
  }

  def fromTo(viewLoc: Point, targetLoc: Point, openingAngle: Double, image: RTBufferedImage): ViewData = {
    val forward = (targetLoc - viewLoc).normalize
    val up = if (forward.x == 0.0 && forward.y == 0.0) Vect(0, 1, 0) else Vect(forward.y, -forward.x, 0).normalize
    new ViewData(View(viewLoc, forward, up), openingAngle, image, Array.fill(image.width, image.height)(RTColor.Black))
  }
}