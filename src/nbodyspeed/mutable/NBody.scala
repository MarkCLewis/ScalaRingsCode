package nbodyspeed.mutable

class Vect3D(var x: Double, var y: Double, var z: Double)
class Particle(val pos: Vect3D, val vel: Vect3D, val mass: Double)

object NBody extends App {
  val NumBodies = 1000
  val dt = 1e-9
  val bodies = Array.fill(NumBodies)(new Particle(new Vect3D(math.random, math.random, math.random),
    new Vect3D(math.random, math.random, math.random), 1e-10))
  val accel = Array.fill(NumBodies)(new Vect3D(0, 0, 0))

  def calcAccelFor(): Unit = {
    for (v <- accel) {
      v.x = 0.0
      v.y = 0.0
      v.z = 0.0
    }
    for (i <- 0 until bodies.length - 1) {
      for (j <- i + 1 until bodies.length) {
        val pi = bodies(i).pos
        val pj = bodies(j).pos
        val dx = pi.x - pj.x
        val dy = pi.y - pj.y
        val dz = pi.z - pj.z
        val dist = math.sqrt(dx * dx + dy * dy + dz * dz)
        val mag = -1 / (dist * dist * dist)
        accel(i).x += bodies(j).mass * dx * mag
        accel(i).y += bodies(j).mass * dy * mag
        accel(i).z += bodies(j).mass * dz * mag
        accel(j).x -= bodies(i).mass * dx * mag
        accel(j).y -= bodies(i).mass * dy * mag
        accel(j).z -= bodies(i).mass * dz * mag
      }
    }
  }

  def calcAccelWhile(): Unit = {

  }

  def stepFor(): Unit = {
    calcAccelFor()
    for (i <- bodies.indices) {
      bodies(i).vel.x += accel(i).x * dt
      bodies(i).vel.y += accel(i).y * dt
      bodies(i).vel.z += accel(i).z * dt
      bodies(i).pos.x += bodies(i).vel.x * dt
      bodies(i).pos.y += bodies(i).vel.y * dt
      bodies(i).pos.z += bodies(i).vel.z * dt
    }
  }

  val start = System.nanoTime()
  for (i <- 1 to 1000) {
    stepFor()
  }
  println(s"Took ${(System.nanoTime() - start) / 1e9} seconds")
}