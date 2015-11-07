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
import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.image.WritableImage
import scalafx.scene.image.ImageView
import scalafx.scene.paint.Color
import scalafx.scene.chart.ScatterChart
import scalafx.scene.chart.NumberAxis
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.XYChart
import scalafx.scene.chart.LineChart
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.HBox
import scalafx.scene.control.MenuBar
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Label
import scalafx.scene.control.TextField
import scalafx.scene.control.Button
import scalafx.event.ActionEvent
import scalafx.scene.layout.FlowPane
import scalafx.scene.control.Menu
import scalafx.scene.control.MenuItem
import scalafx.scene.control.SeparatorMenuItem
import scalafx.stage.FileChooser
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.application.Platform

object SynthOccultations extends JFXApp {
  case class BinData(xmin: Double, xmax: Double, ymin: Double, ymax: Double, binSize: Double, bins: IndexedSeq[IndexedSeq[IndexedSeq[Particle]]])

  case class Photon(x: Double, y: Double, hit: Boolean)
  case class Scan(sx: Double, sy: Double, ex: Double, ey: Double, intensity: Double, photons: Seq[Photon])

  def multipleCuts(x: Double, y: Double, theta: Double, phi: Double, cutTheta: Double, scanLength: Double,
    offLength: Double, beamSize: Double, binData: BinData, zmin: Double, zmax: Double, photons: Int, cutSpread: Double): Seq[Seq[Scan]] = {
    var my = y
    while (my - cutSpread > binData.ymin) my -= cutSpread
    val ret = mutable.ArrayBuffer[Seq[Scan]]()
    while (my < binData.ymax) {
      println("Cut at " + my)
      ret += syntheticOccultation(x, my, theta, phi, cutTheta, scanLength, offLength, beamSize, binData, zmin, zmax, photons)
      my += cutSpread
    }
    ret
  }

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
  def syntheticOccultation(x: Double, y: Double, theta: Double, phi: Double, cutTheta: Double, scanLength: Double,
    offLength: Double, beamSize: Double, binData: BinData, zmin: Double, zmax: Double, photonCount: Int): Seq[Scan] = {
    val rDir = Vect3D(cos(theta) * cos(phi), sin(theta) * cos(phi), sin(phi))
    val dx = math.cos(cutTheta)
    val dy = math.sin(cutTheta)
    val height = zmin.abs max zmax
    val xstart = binData.xmin + rDir.x * height
    val xend = binData.xmax - rDir.x * height
    var mx = xstart
    val ret = mutable.Buffer[Scan]()
    while (mx < xend) {
      val sx = mx
      val sy = y + (math.tan(cutTheta) * (sx - x))
      val ex = sx + scanLength * math.cos(cutTheta)
      val ey = y + (math.tan(cutTheta) * (ex - x))
      val photons = (1 to photonCount).par.map(_ => {
        val t = math.random
        val rx = sx + t * (ex - sx) + math.random * math.random * beamSize
        val ry = sy + t * (ey - sy) + math.random * math.random * beamSize
        Photon(rx, ry, rayGridIntersect(Ray(Vect3D(rx, ry, 0), rDir), binData, zmin, zmax))
      })
      ret += Scan(sx, sy, ex, ey, photons.count(p => !p.hit).toDouble / photonCount, photons.seq)
      mx += (scanLength + offLength) * math.cos(cutTheta)
    }
    ret
  }

  def rayGridIntersect(r: Ray, binData: BinData, zmin: Double, zmax: Double): Boolean = {
    val tmin = (zmin - r.r0.z) / r.r.z
    val tmax = (zmax - r.r0.z) / r.r.z
    val xmin = r.r0.x + tmin * r.r.x
    val xmax = r.r0.x + tmax * r.r.x
    val ymin = r.r0.y + tmin * r.r.y
    val ymax = r.r0.y + tmax * r.r.y
    val minxbin = (((xmin min xmax) - binData.xmin) / binData.binSize).toInt - 1 max 0
    val maxxbin = (((xmin max xmax) - binData.xmin) / binData.binSize).toInt + 1 min binData.bins.length - 1
    val minybin = (((ymin min ymax) - binData.ymin) / binData.binSize).toInt - 1 max 0
    val maxybin = (((ymin max ymax) - binData.ymin) / binData.binSize).toInt + 1 min binData.bins(0).length - 1
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

  var data = CartAndRad.read(new File("/home/mlewis/Rings/JoshCDAP/a=130000:q=2.8:min=2e-9:max=2e-8:rho=0.5:sigma=45/CartAndRad.5000.bin"))

  println("Data read")

  var binData = binParticles(data)

  val (zmin, zmax) = {
    val sorted = data.map(_.z).sorted
    (sorted(100), sorted(sorted.length - 100))
  }

  println(binData.xmin, binData.xmax, binData.ymin, binData.ymax, binData.binSize, binData.bins.length, binData.bins(0).length)

  val length = 100.0
  val gap = 20.0
  val beamWidth = 10.0
  val theta = 0.0
  val phi = 90
  val cutTheta = 0.0
  val spread = 100.0

  stage = new JFXApp.PrimaryStage {
    title = "Show Bins"
    scene = new Scene(1600, 900) {

      //      val wimg = new WritableImage(binData.bins.length, binData.bins(0).length)
      //      val writer = wimg.pixelWriter
      //      for (i <- binData.bins.indices; j <- binData.bins(i).indices) {
      //        if(j==0) println(i)
      //        if (rayGridIntersect(Ray(Vect3D(binData.xmin + i * binData.binSize, binData.ymin + j * binData.binSize, 0), Vect3D(2, 0, 1)), binData, zmin, zmax)) writer.setColor(i, j, Color.Black)
      //        else writer.setColor(i, j, Color.White)
      //      }
      //      content = new ImageView(wimg)

      val occult = new LineChart(NumberAxis(), NumberAxis())
      occult.minWidth = 800
      occult.minHeight = 800

      val canvas = new Canvas(800, 800)
      val gc = canvas.graphicsContext2D

      val grid = new GridPane
      grid.children = List(occult, canvas)
      GridPane.setConstraints(occult, 0, 0)
      GridPane.setConstraints(canvas, 1, 0)

      val menuBar = new MenuBar
      val menu = new Menu("File")
      val openItem = new MenuItem("Open")
      openItem.onAction = (e: ActionEvent) => {
        val chooser = new FileChooser
        val file = chooser.showOpenDialog(stage)
        if (file != null) {
          Future {
            data = CartAndRad.read(file)
            binData = binParticles(data)
            processOccultation()
          }
        }
      }
      val saveImageItem = new MenuItem("Save Image")
      saveImageItem.onAction = (e: ActionEvent) => {
        ???
      }
      val exitItem = new MenuItem("Exit")
      exitItem.onAction = (e: ActionEvent) => {
        sys.exit(0)
      }
      menu.items = List(openItem, saveImageItem, new SeparatorMenuItem, exitItem)
      menuBar.menus += menu

      val lengthField = new TextField
      lengthField.text = length.toString
      val gapField = new TextField
      gapField.text = gap.toString
      val beamWidthField = new TextField
      beamWidthField.text = beamWidth.toString
      val thetaField = new TextField
      thetaField.text = theta.toString
      val phiField = new TextField
      phiField.text = phi.toString
      val cutThetaField = new TextField
      cutThetaField.text = cutTheta.toString
      val spreadField = new TextField
      spreadField.text = spread.toString
      val button = new Button("Process")
      val settings = new FlowPane(10, 10)
      settings.children = List(new Label("Length:"), lengthField, new Label("Gap:"), gapField,
        new Label("Beam Width:"), beamWidthField, new Label("Theta:"), thetaField,
        new Label("Phi:"), phiField, new Label("Cut Theta:"), cutThetaField,
        new Label("Spread:"), spreadField, button)
      button.onAction = (e: ActionEvent) => {
        Future { processOccultation() }
      }

      val border = new BorderPane
      border.top = menuBar
      border.bottom = settings
      border.center = grid

      root = border

      processOccultation()

      def processOccultation(): Unit = {
        val so = multipleCuts(0, 0, thetaField.text.value.toDouble * math.Pi / 180, phiField.text.value.toDouble * math.Pi / 180,
          cutThetaField.text.value.toDouble * math.Pi / 180, lengthField.text.value.toDouble / 136505500, gapField.text.value.toDouble / 136505500,
          beamWidthField.text.value.toDouble / 136505500, binData, zmin, zmax, 1000, spreadField.text.value.toDouble / 136505500)
        val chartData = so.indices.flatMap(i => so(i).map(sc => XYChart.Data[Number, Number]((sc.sx + sc.ex) / 2 + i * (binData.xmax - binData.xmin), sc.intensity)))
        Platform.runLater {
          occult.data = ObservableBuffer(XYChart.Series(ObservableBuffer(chartData: _*)))
          drawCanvas(so)
        }
      }

      def drawCanvas(scanData: Seq[Seq[Scan]]): Unit = {
        gc.fill = Color.White
        gc.fillRect(0, 0, 800, 800)
        gc.save
        gc.translate(400, 400)
        gc.scale(800 / (binData.xmax - binData.xmin), -800 / (binData.ymax - binData.ymin))
        gc.fill = Color.Black
        for (p <- data) {
          gc.fillOval(p.x - p.rad, p.y - p.rad, p.rad * 2, p.rad * 2)
        }
        val size = (binData.xmax - binData.xmin) / 1600
        for (slice <- scanData; scan <- slice; photon <- scan.photons) {
          gc.fill = if (photon.hit) Color.Red else Color.Green
          gc.fillOval(photon.x - size, photon.y - size, 2 * size, 2 * size)
        }
        gc.restore
      }
    }
  }
}