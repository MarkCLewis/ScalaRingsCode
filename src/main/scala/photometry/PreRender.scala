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

object PreRender {
  def main(args: Array[String]): Unit = {
    val step = 10000 // what does this do??

    val carURL = new URL("file:///home/mlewis/Rings/DensityWaves/CartAndRad.90500.bin") /*Keaton's file*/
    //val carURL = new URL("file:///home/lizzie/workspace/RingsResearch/v1.0E-6,oa-45,va90/CartAndRad.0.bin")
    val lights = List(PhotonSource(PointLight(RTColor(1, 1, 1), Point(1, 0, 0.2), Set.empty), 100000))//, PhotonSource(PointLight(new RTColor(1.0, 0.8, 0.2), Point(-1e-1, 0, 1e-2)), 2000))
    
    val threads: Int = 24
    
    //for(i <- 0 to 100) {
      //val carURL = new URL("file:///home/lizzie/workspace/RingsResearch/v1.0E-6,oa-45,va90/CartAndRad." + i*100 + ".bin")
      
      //val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/HighRes/Moonlet4d/CartAndRad." + step.toString + ".bin")
      //val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/MesoScaleFeatures/AGUPosterRun/a=123220:q=2.8:min=15e-9:max=1.5e-8:rho=0.4:sigma=45.5/CartAndRad.4420.bin")
      //val impactURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/HighRes/Moonlet4d/HighVelColls.bin")
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
      //println(particles.length)
      val ringGeom = new KDTreeGeometry[BoundingBox](particles
        .map(p => new ScatterSphereGeom(Point(p.x-centerx, p.y-centery, p.z), p.rad, _ => new RTColor(1, 1, 1, 1), _ => 0.0)), 5, BoxBoundsBuilder)
      println("I am carURL: " + carURL.getContent())
        
      /*val impacts = HighVelocityCollisions.readStream(impactURL.openStream())//.takeRight(1000)
          //println(impacts.last)
          //println(impacts.foldLeft(0)(_ + _.colls.length))
      val impactGeom = new KDTreeGeometry[BoundingSphere](impacts.flatMap(scd => scd.colls
        .map(coll => new ScatterSphereGeom(Point(coll.p1.x, coll.p1.y, coll.p1.z), coll.p1.rad*100 min 1e-7, 
        _ => new RTColor(coll.vel/5e-6 min 1.0, 0, 1.0 - coll.vel/5e-6 min 1.0, 1), _ => 0.0))))
        */
      val dt = math.Pi/500   
      //val dustGeom = new DustGeom(Point(0,0,0), Vect(5e-6, 0, 0), Vect(0, 2e-5, 0), Vect(0, 0, 1e-7), 0.5/1e-6)


      //Uncomment this to render Geometry with Dust
      /*val lastStep = impacts.last.step
      val dustGeom = new KDTreeGeometry[BoundingSphere](impacts.flatMap(scd => scd.colls
        .map(coll => {
          val mag = coll.vel*(coll.p1.rad min coll.p2.rad)
          val size = mag*5e4
          val xmult = 1.0+(lastStep - scd.step)/1000
          val ymult = 1.0+(lastStep - scd.step)/30
          val zmult = 2.0
          new DustGeom(Point(coll.p1.x, coll.p1.y - 1.5*coll.p1.x*dt*(lastStep - scd.step), coll.p1.z), Vect(size*xmult, 0, 0), 
            Vect(0, size*ymult, 0), Vect(0, 0, size*zmult), mag*1e14/(1e-6*xmult*ymult), _ => RTColor.Red)
        })
      )) 
      */

      val geom = new ListScene(ringGeom)//, dustGeom) //, impactGeom)

      val bimg = new BufferedImage(1200, 1200, BufferedImage.TYPE_INT_ARGB)
      val img = new rendersim.RTBufferedImage(bimg)

      val viewLoc = Point(-10e-3, 10e-3, -3e-3)
      val viewData = Seq(ViewData.atOriginFrom(viewLoc, 0.008, img))

      val totalPhotons = 1000000000L
      val maxPasses = math.ceil(totalPhotons.toDouble / (lights.map(_.numPhotons).sum * threads)).toInt
      println(s"Going $maxPasses passes.")

      val frame = new MainFrame {
        title = "Dust Frame"
        contents = new Label("", Swing.Icon(bimg), Alignment.Center)
      }
      frame.visible = true
      val fFinalViews = Render.parallelRender(viewData, 0, 200, threads, lights, geom, Some(frame))
      fFinalViews.foreach { vds =>
        for ((vd, i) <- vds.zipWithIndex) {
          ImageIO.write(vd.image.bimg, "PNG", new java.io.File(s"photoRender.${i.formatted("%04d")}.png"))
        }
      }
    }
  }
//}