package photometry

import scala.swing.{MainFrame, Label, Swing, Alignment}
import swiftvis2.raytrace._
import java.awt.image.BufferedImage
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import javax.imageio._
import java.net.URL
import data.HighVelocityCollisions
import java.io._


object PreRender {
  def main(args: Array[String]): Unit = {
    val step = 10000 // what does this do??

    val carURL = new URL("file:///home/lizzie/workspace/RingsResearch/CartAndRad.84200.bin") /*Keaton's file*/
    //val carURL = new URL("file:///home/lizzie/workspace/RingsResearch/v1.0E-6,oa-45,va90/CartAndRad.0.bin")
    val lights = List(PhotonSource(PointLight(RTColor(1, 1, 1), Point(1, 0, 0.2), Set.empty), 100000), PhotonSource(PointLight(new RTColor(1.0, 0.8, 0.2), Point(-1e-1, 0, 1e-2)), 20000))
    
    val bimg = new BufferedImage(1200, 1200, BufferedImage.TYPE_INT_ARGB)
    val img = new rendersim.RTBufferedImage(bimg)
    val threads: Int = 8
    
    //for(i <- 0 to 100) {
      //val carURL = new URL("file:///home/lizzie/workspace/RingsResearch/v1.0E-6,oa-45,va90/CartAndRad." + i*100 + ".bin")
      
      //val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/HighRes/Moonlet4d/CartAndRad." + step.toString + ".bin")
      //val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/MesoScaleFeatures/AGUPosterRun/a=123220:q=2.8:min=15e-9:max=1.5e-8:rho=0.4:sigma=45.5/CartAndRad.4420.bin")
      //val impactURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/HighRes/Moonlet4d/HighVelColls.bin")
      val particles = data.CartAndRad.readStream(carURL.openStream)//.sortBy(_.x).slice(1000, 10579144) /*This code is for Keaton's giant file*/
      val minx = particles.minBy(_.x).x
      val miny = particles.minBy(_.y).y
      val maxx = particles.maxBy(_.x).x
      val maxy = particles.maxBy(_.y).y
      val centerx = (minx + maxx)/2
      val centery = (miny + maxy)/2
      println(maxx-minx)
      println(maxy-miny)
      //println(particles.length)
      val ringGeom = new KDTreeGeometry[BoundingBox](particles
        //.filter(p => p.y < 3e-5 && p.y > -3e-5)
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

      val frame = new MainFrame {
        title = "Dust Frame"
        contents = new Label("", Swing.Icon(bimg), Alignment.Center)
      }
      frame.visible = true
      Render.parallelRender(Array.fill(img.width, img.height)(RTColor.Black), 0, threads, lights, geom, img, frame)
    }
  }
//}