package simprocessing

import data.CartAndRad
import util.Particle
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import scala.annotation.tailrec

object CartPlotFiltered {
  val ArgRegex = """-([xy]-)?(num|frac|axis|scale)(-min|-max)?""".r
  val FileRegex = """.*CartAndRad.(\d+).bin""".r
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage: CartPlotFiltered [opts] CartAndRad-files")
      println("  -size: Followed by two values for width and height in pixels.")
      println("  -display: Show a window in addition to writing to file.")
      println("  -label-format: C-style format string for axis labels.")
      println("  ? in options can be replaced by x or y.")
      println("  Filters are applied in the ordr specified.")
      println("  -?-num[-min|-max]: Drop off next argument number of particles.")
      println("  -?-frac[-min|-max]: Drop off next argument fraction of particles.")
      println("  -?-axis-(min|max): Fix the axis value as specified. Also does a filter.")
      return
    }
    val display = args.contains("-display")
    val (width, height) = if (args.contains("-size")) {
      val i = args.indexOf("-size")
      (args(i+1).toInt, args(i+2).toInt)
    } else (1200, 1200)
    val labelFormat = if (args.contains("-label-format")) {
      val i = args.indexOf("-label-format")
      args(i+1)
    } else "%1.1f"
    val (transforms, sources) = processArgs(args.toList, Nil, Nil)
    val xaxisMinIndex = args.indexOf("-x-axis-min")
    val xaxisMin = if(xaxisMinIndex < 0) None else Some(args(xaxisMinIndex + 1).toDouble)
    val xaxisMaxIndex = args.indexOf("-x-axis-max")
    val xaxisMax = if(xaxisMaxIndex < 0) None else Some(args(xaxisMaxIndex + 1).toDouble)
    val yaxisMinIndex = args.indexOf("-y-axis-min")
    val yaxisMin = if(yaxisMinIndex < 0) None else Some(args(yaxisMinIndex + 1).toDouble)
    val yaxisMaxIndex = args.indexOf("-y-axis-max")
    val yaxisMax = if(yaxisMaxIndex < 0) None else Some(args(yaxisMaxIndex + 1).toDouble)
    
    val ptrans = transforms.reverse
    for(s <- sources) {
      val FileRegex(fnum) = s
      val parts = ptrans.foldLeft(CartAndRad.read(new java.io.File(s)))((ps, t) => t(ps))
      val rads = parts.map(_.rad*2)
      val plot = Plot.simple(ScatterStyle(parts.map(_.x), parts.map(_.y), symbolWidth = rads, symbolHeight = rads, xSizing = PlotSymbol.Sizing.Scaled, ySizing = PlotSymbol.Sizing.Scaled), xLabel = "Radial", yLabel = "Azimuthal").
        updatedAxis[NumericAxis]("x", na => na.copy(min = xaxisMin, max = xaxisMax, tickLabelInfo = na.tickLabelInfo.map(_.copy(numberFormat = labelFormat)))).
        updatedAxis[NumericAxis]("y", na => na.copy(min = yaxisMin, max = yaxisMax, tickLabelInfo = na.tickLabelInfo.map(_.copy(numberFormat = labelFormat))))
      SwingRenderer.saveToImage(plot, "output."+("0"*(7-fnum.length))+fnum+".png", width = width, height = height)
      if (display) {
        SwingRenderer(plot, width, height)
      }
    }
  }

  @tailrec
  def processArgs(args: List[String], trans: List[IndexedSeq[Particle] => IndexedSeq[Particle]], files: List[String]): (List[IndexedSeq[Particle] => IndexedSeq[Particle]], List[String]) = args match {
    case Nil => (trans, files)
    case ArgRegex(xy, style, minmax) :: n :: t =>
      val num = n.toDouble
      val otran: Option[IndexedSeq[Particle] => IndexedSeq[Particle]] = style match {
        case "num" =>
          if (xy == null) {
            println("num requires x/y specification")
            None
          } else {
            val cartFunc: Particle => Double = if (xy == "x-") p => p.x else p => p.y
            Some(ps => ps.sortBy(cartFunc).drop(if (minmax == "-min" || minmax == null) n.toInt else 0).dropRight(if (minmax == "-max" || minmax == null) n.toInt else 0))
          }
        case "frac" =>
          if (xy == null) {
            println("frac requires x/y specification")
            None
          } else {
            val cartFunc: Particle => Double = if (xy == "x-") p => p.x else p => p.y
            Some(ps => ps.sortBy(cartFunc).drop(if (minmax == "-min" || minmax == null) (num*ps.length).toInt else 0).dropRight(if (minmax == "-max" || minmax == null) (num*ps.length).toInt else 0))
          }
        case "axis"  =>
          if (xy == null) {
            println("frac requires x/y specification")
            None
          } else if(minmax == null) {
            println("axis require min or max")
            None
          } else {
            val cartFunc: Particle => Double = if (xy == "x-") p => p.x else p => p.y
            val mmFunc: Double => Boolean = if (minmax == "-min") x => x > num else x => x < num 
            Some(ps => ps.filter(p => mmFunc(cartFunc(p))))
          }
        case _ => None
      }
      val newTrans = otran.map(_ :: trans).getOrElse(trans)
      processArgs(t, newTrans, files)
    case (fname@FileRegex(num)) :: t =>
      processArgs(t, trans, fname :: files)
    case _ :: t =>
      processArgs(t, trans, files)
  }
}