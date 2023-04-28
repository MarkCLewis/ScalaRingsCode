package chariklosims

import util.Particle
import collection.mutable
import scalafx.scene.input.KeyCode

class OverlapFinder(data: IndexedSeq[Particle], firstIndex: Int) {
  private val MinGridCount = 20
  private val parts = mutable.Buffer(firstIndex)
  private var grid: mutable.Map[Int, mutable.Map[Int, List[Int]]] = null
  private var gridCellSize = -1.0

  def checkParticle(i: Int): Boolean = {
    if (grid == null) parts.exists(inc => data(i).overlapped(data(inc)))
    else {
      val gx = ((data(i).x - data(firstIndex).x) / gridCellSize).toInt
      val gy = ((data(i).y - data(firstIndex).y) / gridCellSize).toInt
      return (-1 to 1).exists { dgx => (-1 to 1).exists { dgy =>
        grid.contains(gx + dgx) && grid(gx + dgx)(gy + dgy).exists { pi =>
          data(i).overlapped(data(pi))
        }
      }}
    }
  }

  def addParticle(i: Int): Unit = {
    parts += i
    if (parts.length >= MinGridCount) {
      if (grid == null) {
        grid = mutable.Map.empty[Int, mutable.Map[Int, List[Int]]]
        gridCellSize = data(parts.maxBy(data(_).rad)).rad * 2
        for (pi <- parts) addToGrid(pi)
      } else {
        addToGrid(i)
      }
    }
  }

  private def addToGrid(i: Int): Unit = {
    val gx = ((data(i).x - data(firstIndex).x) / gridCellSize).toInt
    val gy = ((data(i).y - data(firstIndex).y) / gridCellSize).toInt
    if (!grid.contains(gx)) {
      grid(gx) = mutable.Map.empty[Int, List[Int]].withDefaultValue(Nil)
    }
    grid(gx)(gy) ::= i
  }
}