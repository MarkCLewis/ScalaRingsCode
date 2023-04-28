package photometry

import scala.swing.{MainFrame, Label, Swing, Alignment, TabbedPane}
import swiftvis2.raytrace._
import java.awt.image.BufferedImage
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import javax.imageio._
import java.net.URL
import data.HighVelocityCollisions
import java.io._
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer

object FixedPathRender {
  def main(args: Array[String]): Unit = {
    val carURL = new URL("file:///home/mlewis/Rings/DensityWaves/CartAndRad.1999500.bin") /*Keaton's file*/

    val lights = List(PhotonSource(PointLight(RTColor(1, 1, 1), Point(1, 0, 0.2), Set.empty), 100000))//, PhotonSource(PointLight(new RTColor(1.0, 0.8, 0.2), Point(-1e-1, 0, 1e-2)), 2000))
    
    val threads: Int = 24
    
    val rawParticles = data.CartAndRad.readStream(carURL.openStream)
    val centerParticles = rawParticles.sortBy(_.x).slice(1000, rawParticles.length-1000) /*This code is for Keaton's giant file*/
    val minx = centerParticles.minBy(_.x).x
    val miny = centerParticles.minBy(_.y).y
    val maxx = centerParticles.maxBy(_.x).x
    val maxy = centerParticles.maxBy(_.y).y
    val centerx = (minx + maxx)/2 + 4e-5
    val centery = (miny + maxy)/2 - 4e-5
    println(maxx-minx)
    println(maxy-miny)
    val particles = centerParticles//.filter(p => (p.x - centerx).abs < 1e-4)// && (p.y - centery).abs < 5e-5)
    // val plot = Plot.scatterPlot(
    //     particles.map(_.x - centerx), 
    //     particles.map(_.y - centery), 
    //     symbolSize = particles.map(_.rad*2), 
    //     xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled)
    //   .updatedAxis[NumericAxis]("x", _.updatedName("x").numberFormat("%1.5f"))
    //   .updatedAxis[NumericAxis]("y", _.updatedName("y").numberFormat("%1.5f"))
    // SwingRenderer(plot, 1200, 1200, true)
    val ringGeom = new KDTreeGeometry[BoundingBox](particles
      .map(p => new ScatterSphereGeom(Point(p.x-centerx, p.y-centery, p.z), p.rad, _ => new RTColor(1, 1, 1, 1), _ => 0.0)), 5, BoxBoundsBuilder)
        
    val geom = new ListScene(ringGeom)//, dustGeom) //, impactGeom)

    val xyDist = 15e-3
    val zDist = -4e-3
    val openingAngle = 0.008
    val framesAround = 20
    val viewData = for (angInt <- 0 until 7) yield {
      val bimg = new BufferedImage(1200, 1200, BufferedImage.TYPE_INT_ARGB)
      val img = new rendersim.RTBufferedImage(bimg)
      val ang = (math.Pi * 2 * angInt / framesAround)
      val viewLoc = Point(xyDist * math.cos(ang), xyDist*math.sin(ang), zDist)
      ViewData.atOriginFrom(viewLoc, openingAngle, img)
    }

    val totalPhotons = 500000000L
    val maxPasses = math.ceil(totalPhotons.toDouble / (lights.map(_.numPhotons).sum * threads)).toInt
    println(s"Going $maxPasses passes.")

    val frame = new MainFrame {
      title = "Dust Frame"
      val tabbedPane = new TabbedPane()
      for ((vd, i) <- viewData.zipWithIndex) {
        tabbedPane.pages += new TabbedPane.Page(i.toString, new Label("", Swing.Icon(vd.image.bimg), Alignment.Center))
      }
      contents = tabbedPane
    }
    frame.visible = true
    val fFinalViews = Render.parallelRender(viewData, 0, maxPasses, threads, lights, geom, Some(frame))
    fFinalViews.foreach { vds =>
      for ((vd, i) <- vds.zipWithIndex) {
        ImageIO.write(vd.image.bimg, "PNG", new java.io.File(s"photoRender.${"%04d".format(i)}.png"))
      }
    }
  }
}
