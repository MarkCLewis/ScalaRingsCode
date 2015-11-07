package occultations

import java.io.File
import scala.collection.mutable
import scala.math.cos
import scala.math.sin
import data.CartAndRad
import scalafx.application.JFXApp
import util.Particle
import util.Ray
import util.Vect3D
import scalafx.scene.Scene
import scalafx.scene.image.WritableImage
import scalafx.scene.image.ImageView
import scalafx.scene.paint.Color
import scalafx.scene.chart.ScatterChart
import scalafx.scene.chart.NumberAxis
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.XYChart

object SynthOccultations extends JFXApp {
  case class BinData(xmin: Double, xmax: Double, ymin: Double, ymax: Double, binSize: Double, bins: IndexedSeq[IndexedSeq[IndexedSeq[Particle]]])
  
  case class Scan(sx:Double, sy:Double, ex:Double, ey:Double, intensity:Double)
  
  /**
   * @param x The radial coordinate of a point the occultation crosses in the plane
   * @param y The azimuthal coordinate of a point the occultation crosses in the plane
   * @param theta The angle in the plane from the radial axis of the observation
   * @param phi The elevation angle out of the plane of the observation
   * @param cutTheta The angle in the plane from the radial of the motion of the beam in the plane
   * @param scanLength How long each scan covers when the shutter is open
   * @param offLength How long the scan moves when the shutter is closed
   * @param beamSize The radius of the beam in the ring plane
   */
  def syntheticOccultation(x:Double, y:Double, theta: Double, phi: Double, cutTheta:Double, scanLength:Double, 
      offLength:Double, beamSize:Double, binData: BinData, zmin: Double, zmax: Double, photons:Int):Seq[Scan] = {
    val rDir = Vect3D(cos(theta) * cos(phi), sin(theta) * cos(phi), sin(phi))
    val dx = math.cos(cutTheta)
    val dy = math.sin(cutTheta)
    val height = zmin.abs max zmax
    val xstart = binData.xmin+rDir.z*height
    val xend = binData.xmax-rDir.z*height
    var mx = xstart
    val ret = mutable.Buffer[Scan]()
    while(mx < xend) {
      val sx = mx
      val sy = y+(math.tan(cutTheta)*(sx-x))
      val ex = sx+scanLength*math.cos(cutTheta)
      val ey = y+(math.tan(cutTheta)*(ex-x))
      val cnt = (1 to photons).count(_ => {
        val t = math.random
        val rx = sx+t*(ex-sx)+math.random*beamSize
        val ry = sx+t*(ex-sx)+math.random*beamSize
        !rayGridIntersect(Ray(Vect3D(rx,ry,0),rDir), binData, zmin, zmax)
      })
      ret += Scan(sx,sy,ex,ey,cnt.toDouble/photons)
      mx += (scanLength+offLength)*math.cos(cutTheta)
    }
    ret
  }

  def makeRay(x: Double, y: Double, theta: Double, phi: Double): Ray = {
    import math._
    Ray(Vect3D(x, y, 0), Vect3D(cos(theta) * cos(phi), sin(theta) * cos(phi), sin(phi)))
  }

  def rayGridIntersect(r: Ray, binData: BinData, zmin: Double, zmax: Double): Boolean = {
    val tmin = (zmin - r.r0.z) / r.r.z
    val tmax = (zmax - r.r0.z) / r.r.z
    val xmin = r.r0.x + tmin * r.r.x
    val xmax = r.r0.x + tmax * r.r.x
    val ymin = r.r0.y + tmin * r.r.y
    val ymax = r.r0.y + tmax * r.r.y
    val minxbin = (((xmin min xmax) - binData.xmin) / binData.binSize).toInt max 0
    val maxxbin = (((xmin max xmax) - binData.xmin) / binData.binSize).toInt min binData.bins.length-1
    val minybin = (((ymin min ymax) - binData.ymin) / binData.binSize).toInt max 0
    val maxybin = (((ymin max ymax) - binData.ymin) / binData.binSize).toInt min binData.bins(0).length-1
    (minxbin to maxxbin).exists(xbin => {
      (minybin to maxybin).exists(ybin => rayBinIntersect(r, xbin, ybin, binData))
    })
  }

  def rayBinIntersect(r: Ray, xbin: Int, ybin: Int, binData: BinData): Boolean = {
    binData.bins(xbin)(ybin).exists(p => rayParticleIntersect(r, p))
  }

  def rayParticleIntersect(r: Ray, part: Particle): Boolean = {
    val p = new Vect3D(part.x, part.y, part.z)
    val d = r.r0 - p
    val a = r.r dot r.r
    val b = 2 * (r.r dot d)
    val c = (d dot d) - part.rad * part.rad
    b * b - 4 * a * c >= 0
  }

  def binParticles(parts: IndexedSeq[Particle]): BinData = {
    val (xmin, xmax, ymin, ymax) = parts.foldLeft(Double.MaxValue, Double.MinValue, Double.MaxValue, Double.MinValue)((acc, p) => {
      (acc._1 min p.x, acc._2 max p.y, acc._3 min p.y, acc._4 max p.y)
    })
    val dx = xmax - xmin
    val dy = ymax - ymin
    val partsPerBin = 1.0
    val binSize = math.sqrt(dx * dy * partsPerBin / parts.length)
    val ret = Array.fill(math.ceil(dx / binSize).toInt)(Array.fill(math.ceil(dy / binSize).toInt)(mutable.ArrayBuffer[Particle]()): IndexedSeq[mutable.ArrayBuffer[Particle]])
    for (i <- parts.indices) {
      val binx = ((parts(i).x - xmin) / binSize).toInt min ret.length - 1
      val biny = ((parts(i).y - ymin) / binSize).toInt min ret(0).length - 1
      ret(binx)(biny) += parts(i)
    }
    BinData(xmin, xmax, ymin, ymax, binSize, ret)
  }

  val data = CartAndRad.read(new File("/home/mlewis/Rings/JoshCDAP/a=130000:q=2.8:min=2e-9:max=2e-8:rho=0.5:sigma=45/CartAndRad.5000.bin"))

  println("Data read")

  val binData = binParticles(data)

  def binDataf = binData

//  val (zmin, zmax) = data.foldLeft(Double.MaxValue, Double.MinValue)((acc, p) => {
//    (acc._1 min p.z - p.rad, acc._2 max p.z + p.rad)
//  })
  
  val (zmin, zmax) = {
    val sorted = data.map(_.z).sorted
    (sorted(100), sorted(sorted.length-100))
  }

  println(binData.xmin, binData.xmax, binData.ymin, binData.ymax, binData.binSize, binData.bins.length, binData.bins(0).length)
  val so = syntheticOccultation(0, 0, 0, math.Pi/2, 0, 100.0/136505500, 10.0/136505500, 5.0/136505500, binData, zmin, zmax, 1000)

  stage = new JFXApp.PrimaryStage {
    title = "Show Bins"
    scene = new Scene(binData.bins.length, binData.bins(0).length) {

//      val wimg = new WritableImage(binData.bins.length, binData.bins(0).length)
//      val writer = wimg.pixelWriter
//      for (i <- binData.bins.indices; j <- binData.bins(i).indices) {
//        if(j==0) println(i)
//        if (rayGridIntersect(Ray(Vect3D(binData.xmin + i * binData.binSize, binData.ymin + j * binData.binSize, 0), Vect3D(2, 0, 1)), binData, zmin, zmax)) writer.setColor(i, j, Color.Black)
//        else writer.setColor(i, j, Color.White)
//      }
//      content = new ImageView(wimg)
      
      val chartData = so.map(sc => XYChart.Data[Number,Number]((sc.sx+sc.ex)/2,sc.intensity))
      val scatter = new ScatterChart(NumberAxis(), NumberAxis(), ObservableBuffer(XYChart.Series(ObservableBuffer(chartData:_*))))
      root = scatter
    }
  }
}