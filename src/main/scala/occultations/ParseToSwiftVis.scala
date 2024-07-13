package occultations

import java.io.PrintWriter

object ParseToSwiftVis extends App {
  val inFilename = if (args.length < 1) "occultations.txt" else args(0)
  val outFilename = if (args.length < 2) "occultationsSV.txt" else args(1)
  
  val DataLine = """(\d+)\t(\d+)\t(\d+)\t(.+)""".r
  
  var indexCount = 0
  val lines = io.Source.fromFile(inFilename).getLines()
  val pw = new PrintWriter(outFilename)
  for(line <- lines) {
    if(line.startsWith("Index")) indexCount += 1
    else {
      line match {
        case DataLine(index, photons, transmitted, intensity) =>
          pw.println(s"$indexCount $index  $photons $transmitted $intensity")
        case _ =>
      }
    }
  }
}