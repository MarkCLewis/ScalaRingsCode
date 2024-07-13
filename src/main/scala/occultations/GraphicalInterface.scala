package occultations

import scalafx.Includes._
import scalafx.application.JFXApp3
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
import java.io.PrintWriter
import scalafx.scene.effect.BlendMode
import java.io.File
import data.CartAndRad

object GraphicalInterface extends JFXApp3 {
  def start(): Unit = {
    val chooser = new FileChooser
    var data = CartAndRad.read(chooser.showOpenDialog(null))
  //  var data = CartAndRad.read(new File("/data/mlewis/Rings/JoshCDAP15-17/a=130000:q=2.8:min=2e-9:max=2e-8:rho=0.5:sigma=30/CartAndRad.30000.bin"))
    //  var data = CartAndRad.read(new File("/home/mlewis/Rings/KeelerGap/Straw60gcm2/CartAndRad.18000.bin"),-1700/136505.5,-1300/136505.5)

    println("Data read")

    var binData = SynthOccultations.binParticles(data)
    var so: Seq[Seq[SynthOccultations.Scan]] = Nil

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
    val spread = 50000 //500.0

    stage = new JFXApp3.PrimaryStage {
      title = "Show Bins"
      scene = new Scene(1600, 850) {

        //      val wimg = new WritableImage(binData.bins.length, binData.bins(0).length)
        //      val writer = wimg.pixelWriter
        //      for (i <- binData.bins.indices; j <- binData.bins(i).indices) {
        //        if(j==0) println(i)
        //        if (rayGridIntersect(Ray(Vect3D(binData.xmin + i * binData.binSize, binData.ymin + j * binData.binSize, 0), Vect3D(2, 0, 1)), binData, zmin, zmax)) writer.setColor(i, j, Color.Black)
        //        else writer.setColor(i, j, Color.White)
        //      }
        //      content = new ImageView(wimg)

        val occult = new LineChart(NumberAxis("Radial Distance [km]"), NumberAxis("Fractional Transmission"))
        occult.minWidth = 800
        occult.minHeight = 800

        val canvas = new Canvas(800, 800)
        val gc = canvas.graphicsContext2D
        //      gc.globalBlendMode = BlendMode.Overlay

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
              binData = SynthOccultations.binParticles(data)
              processOccultation()
            }
          }
        }
        val saveItem = new MenuItem("Save Scans")
        saveItem.onAction = (e: ActionEvent) => {
          val chooser = new FileChooser
          val file = chooser.showSaveDialog(stage)
          if (file != null) {
            Future {
              val pw = new PrintWriter(file)
              for (i <- so.indices; scan <- so(i)) {
                pw.println(s"$i ${scan.sx} ${scan.sy} ${scan.ex} ${scan.ey} ${scan.intensity}")
              }
              pw.close
            }
          }
        }
        val exitItem = new MenuItem("Exit")
        exitItem.onAction = (e: ActionEvent) => {
          sys.exit(0)
        }
        menu.items = List(openItem, saveItem, new SeparatorMenuItem, exitItem)
        menuBar.menus += menu

        val lengthField = new TextField
        lengthField.text = length.toString
        lengthField.prefWidth = 100
        val gapField = new TextField
        gapField.text = gap.toString
        gapField.prefWidth = 100
        val beamWidthField = new TextField
        beamWidthField.text = beamWidth.toString
        beamWidthField.prefWidth = 100
        val thetaField = new TextField
        thetaField.text = theta.toString
        thetaField.prefWidth = 100
        val phiField = new TextField
        phiField.text = phi.toString
        phiField.prefWidth = 100
        val cutThetaField = new TextField
        cutThetaField.text = cutTheta.toString
        cutThetaField.prefWidth = 100
        val spreadField = new TextField
        spreadField.text = spread.toString
        spreadField.prefWidth = 100
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

        //      occult.prefWidth <== scene.width/2
        //      occult.prefHeight <== scene.height-settings.height-menuBar.height
        //      canvas.width <== scene.width/2
        //      canvas.height <== scene.height-settings.height-menuBar.height

        processOccultation()

        def processOccultation(): Unit = {
          so = SynthOccultations.multipleCuts(0, 0, thetaField.text.value.toDouble * math.Pi / 180, phiField.text.value.toDouble * math.Pi / 180,
            cutThetaField.text.value.toDouble * math.Pi / 180, lengthField.text.value.toDouble / 136505500, gapField.text.value.toDouble / 136505500,
            beamWidthField.text.value.toDouble / 136505500, zmax-zmin, binData, 10000, spreadField.text.value.toDouble / 136505500, 2000)
          val chartData = so.indices.flatMap(i => so(i).map(sc => XYChart.Data[Number, Number](((sc.sx + sc.ex) / 2 + i * (binData.xmax - binData.xmin)) * 136505.5, sc.intensity)))
          Platform.runLater {
            occult.data = ObservableBuffer(XYChart.Series("Intensity", ObservableBuffer(chartData: _*)))
            drawCanvas(so)
          }
        }

        def drawCanvas(scanData: Seq[Seq[SynthOccultations.Scan]]): Unit = {
          println(f"${canvas.width.value} ${canvas.height.value}")
          gc.fill = Color.White
          gc.fillRect(0, 0, canvas.width.value, canvas.height.value)
          gc.save()
          gc.translate(canvas.width.value / 2, canvas.height.value / 2)
          gc.scale(canvas.width.value / (binData.xmax - binData.xmin), -canvas.height.value / (binData.ymax - binData.ymin))
          gc.translate(-(binData.xmax + binData.xmin) * 0.5, -(binData.ymax + binData.ymin) * 0.5)
          gc.fill = Color.Black
          println("Draw particles")
          for (p <- data) {
            gc.fillOval(p.x - p.rad, p.y - p.rad, p.rad * 2, p.rad * 2)
          }
          println("Draw photons")
          val size = 0.5 * (binData.xmax - binData.xmin) / canvas.width.value
          for (slice <- scanData; scan <- slice; photon <- scan.photons) {
            gc.fill = if (photon.hit) Color.Red else Color.Green
            gc.fillOval(photon.x - size, photon.y - size, 2 * size, 2 * size)
          }
          println("Done drawing")
          gc.restore()
        }
      }
    }
  }
}