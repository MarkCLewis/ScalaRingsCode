package simprocessing

import java.io.File
import java.io.FilenameFilter
import data.CartAndRad
import util.Particle
import util.GCCoord
import scala.sys

object CompareCartAndRad extends App {
    println(args.length)
    if (args.length < 2) {
        println("ERROR: Must have two CartAndRad files to compare.")
        sys.exit(0)
    } else {
        // Took these lines from ProcessCartAndRad.scala to process the files
        // cartandrad.read both files
        println("Starting up")
        val files = new File(args(0)).listFiles((d: File, s: String) => s.startsWith("CartAndRad"))
        println(files.length)

    }

}
