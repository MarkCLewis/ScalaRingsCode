package simprocessing
import org.apache.commons.math3.distribution.PoissonDistribution
import java.io.File
import data.CartAndRad
import occultations.SynthOccultations
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.styles.ScatterStyle.LineData
import swiftvis2.plotting.renderer.SwingRenderer

object AzimuthalSamplingOccultations {
	def main(args: Array[String]): Unit = {
		if (args.length < 2) {
			println("""
Usage: AzimuthalSamplingOccultations [flags] outputFile dir(s)
	
	This program will go into the specified directories and find the CartAndRad
	files. Beginning with the highest numbered output it will go backwards at
	at particular interval and run a series of occultations across the middle
	of each cell at various opening angles.

	flag options:
		-stepRange=N
			Total number of steps to go back from the final step in the simulation.
			(default: 1000)
		-stepInterval=S
			Minimum number of steps between analyzed data files. (default: 100)
		-bmin=T
			Minimum opening angle in degrees. (default: 5)
		-bmax=T
			Maximum opening angle in degrees. (default: 90)
		-bStep=S
			Number of degrees to step down from max toward min (default: 5)
		-R0=R
			Value of R0 for this simulation is units of users choice. 
			(default: 100000)
		-scanLength=L
			Length of each scan segment in the occultation in same units as R0.
			(default: 0.01)
		-scanWidth=W
			Scan width of occultations in same units as R0. (default: 1e-3)
		-numPhotons=N
			Number of photons sent in each scan. (default: 1000)
		-photonCountMethod=M
			The method of picking the number of photons. Options are:
				constant
				poisson (default)
		-dropFraction=F
			Will drop F*N particles from all edges of the cell to exclude extreme
			outliers and improve binning. (default: 1e-3)
		-showPlots
""".trim())
			sys.exit(0)
		}

		val fileArgs = args.dropWhile(_(0) == '-')
		val pw = new java.io.PrintWriter(fileArgs(0))
		val dirs = fileArgs.drop(1)
		val stepRange = argValue(args, "-stepRange", _.toInt, 1000)
		val stepInterval = argValue(args, "-stepInterval", _.toInt, 100)
		val bmin = argValue(args, "-bmin", _.toInt, 5)
		val bmax = argValue(args, "-bmax", _.toInt, 90)
		val bStep = argValue(args, "-bStep", _.toInt, 5)
		val r0 = argValue(args, "-R0", _.toDouble, 1e5)
		val scanLength = argValue(args, "-scanLength", _.toDouble, 0.01)/r0
		val scanWidth = argValue(args, "-scanWidth", _.toDouble, 1e-3)/r0
		val numPhotons = argValue(args, "-numPhotons", _.toInt, 1000)
		val pd = new PoissonDistribution(numPhotons)
		val photonFunc = argValue(args, "-photonCountMethod", _ match {
			case "constant" => () => numPhotons
			case _ =>	() => pd.sample
		}, () => pd.sample)
		val dropFraction = argValue(args, "-dropFraction", _.toDouble, 1e-3)
		val showPlots = args.contains("-showPlots")
		
		for (dir <- dirs) {
			val dirFile = new File(dir)
			pw.println(s"Directory: $dir")
			val allCartFiles = CartAndRad.findAllInDir(dirFile).sortBy(-_._2)
			if (allCartFiles.nonEmpty) {
				val lastStep = allCartFiles.head._2
				val cartFiles = allCartFiles.takeWhile(_._2 >= lastStep - stepRange).foldLeft(List[(String, Int)]()) { (acc, next) => 
					if (acc.isEmpty) List(next)
					else if (next._2 <= acc.head._2 - stepInterval) next :: acc
					else acc
				}
				for ((fname, step)  <- cartFiles) yield {
					pw.println(s"Step: $step ${lastStep - step}")
					val allParticles = CartAndRad.read(new File(dirFile, fname))
					val ndrop = (allParticles.length*dropFraction).toInt
					val coreParticles = allParticles.sortBy(_.x).drop(ndrop).dropRight(ndrop).sortBy(_.y).drop(ndrop).dropRight(ndrop)
					val xmid = coreParticles.sortBy(_.x).apply(coreParticles.length/2).x
					val ymid = coreParticles.sortBy(_.y).apply(coreParticles.length/2).y
					val (zmin, zmax) = {
						val sorted = coreParticles.sortBy(_.z)
						(sorted(100).z, sorted(sorted.length - 100).z)
					}
					val binData = SynthOccultations.binParticles(coreParticles)
					val scans = for (b <- bmax to bmin by -bStep) yield {
						pw.println(s"b: $b")
						val synthData = SynthOccultations.syntheticOccultation(xmid, ymid, math.Pi/2, b * math.Pi / 180, 0, scanLength, 0.0, scanWidth, zmax-zmin, binData, photonFunc())
						for ((scan, i) <- synthData.zipWithIndex) {
							pw.println(s"$i\t${scan.photons.length}\t${scan.photons.count(!_.hit)}\t${scan.intensity}\t${scan.sx}\t${scan.sy}\t${scan.ex}\t${scan.ey}")
						}
						(synthData, b)
					}
					if (showPlots) {
						val cg = ColorGradient(0.0 -> BlueARGB, 90.0 -> GreenARGB)
						val radii = coreParticles.map(_.rad * 2 *r0)
						val plot = Plot.stacked(
							ScatterStyle(coreParticles.map(_.x * r0), coreParticles.map(_.y * r0), symbolWidth = radii, symbolHeight = radii, xSizing = PlotSymbol.Sizing.Scaled,
								ySizing = PlotSymbol.Sizing.Scaled, colors = BlackARGB) +:
							scans.map { case (scan, b) =>
									ScatterStyle(scan.map(s => (s.sx + s.ex)/2*r0), scan.map(s => (s.ey + (s.photons.count(!_.hit).toDouble / numPhotons) * (binData.ymax - binData.ymin) * 0.4) * r0),
										symbol = NoSymbol, 
										lines = Some(LineData(0)),
										colors = cg(b))
							}
						)
						SwingRenderer(plot, 1000, 1000, true)
					}
				}
			}
		}
		pw.close()
	}

	def argValue[A](args: Array[String], argName: String, f: String => A, default: A): A = {
		args.find(_.startsWith(argName)).map(_.dropWhile(_ != '=').drop(1)).map(f).getOrElse(default)
	}
}