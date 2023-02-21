package chariklosims

import util.Particle
import collection.mutable

class ParticleGrid(data: IndexedSeq[Particle]) {
  val (xmin, xmax, ymin, ymax, maxRad) = {
    var xmin, ymin = 1e100
    var xmax, ymax, maxRad = -1e100
    for (p <- data) {
      if (p.x < xmin) xmin = p.x
      if (p.x > xmax) xmax = p.x
      if (p.y < ymin) ymin = p.y
      if (p.y > ymax) ymax = p.y
      if (p.rad > maxRad) maxRad = p.rad
    }
    (xmin, xmax, ymin, ymax, maxRad)
  }

  println(s"Max rad = $maxRad")

  val cellSize = 5.0 * maxRad
  private val _cells = Array.fill(((xmax-xmin) / cellSize).ceil.toInt, ((ymax-ymin) / cellSize).ceil.toInt)(mutable.Buffer[Int]())
  println(s"Grid size = ${_cells.length} by ${_cells.head.length}")

  def xCell(x: Double): Int = ((x - xmin) / cellSize).toInt max 0 min _cells.length
  def yCell(y: Double): Int = ((y - ymin) / cellSize).toInt max 0 min _cells(0).length

  for (i <- data.indices) {
    val p = data(i)
    _cells(xCell(p.x))(yCell(p.y)) += i
  }

  var cnt = 0
  for(row <- _cells; cell <- row) cnt += cell.length
  println(s"Total sum $cnt")

  val sortedCells = (for (i <- _cells.indices; j <- _cells(i).indices) yield (i, j, _cells(i)(j).length)).sortBy(-_._3)
  def cellIndices(i: Int, j: Int): Seq[Int] = if (i >= 0 && i < _cells.length && j >= 0 && j < _cells(i).length) _cells(i)(j) else Nil
}
