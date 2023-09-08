package util

case class Particle(x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double, rad: Double) {
  def distance(p: Particle): Double = {
    val dx = x - p.x
    val dy = y - p.y
    val dz = z - p.z
    math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  def distSqr(p: Particle): Double = {
    val dx = x - p.x
    val dy = y - p.y
    val dz = z - p.z
    dx * dx + dy * dy + dz * dz
  }

  def distSqr(px: Double, py: Double, pz: Double): Double = {
    val dx = x - px
    val dy = y - py
    val dz = z - pz
    dx * dx + dy * dy + dz * dz
  }

  def overlapped(p: Particle): Boolean = distSqr(p) <= (rad + p.rad) * (rad + p.rad)

  def overlapped(p: Particle, tolerance: Double): Boolean = distSqr(p) <= (rad + p.rad) * (rad + p.rad) * tolerance * tolerance

  def advance(dt: Double): Particle = {
    GCCoord(this).advance(dt).toCart
  }
}

case class GCCoord(X: Double, Y: Double, e: Double, phi: Double, i: Double, zeta: Double, rad: Double) {
  def advance(dt: Double): GCCoord = {
    copy(Y = Y - 1.5 * X * dt, phi = phi + dt, zeta = zeta + dt)
  }

  def toCart: Particle = {
    val cosp=math.cos(phi)
    val sinp=math.sin(phi)
    val x = X-e*cosp
    val y = Y+2.0*e*sinp
    val z = i*math.cos(zeta)
    val vx = e*sinp
    val vy = 2.0*e*cosp-1.5*X
    val vz = -i*math.sin(zeta)
    Particle(x, y, z, vx, vy, vz, rad)
  }
}

object GCCoord {
  val BETA = 2.0

  def apply(p: Particle): GCCoord = {
    val X = (2.0 * p.x + p.vy) * 2.0;
    val Y = p.y - p.vx * BETA;
    val dx = X - p.x;
    val dy = p.y - Y;
    val e = math.sqrt(dx * dx + dy * dy / (BETA * BETA));
    val phi = math.atan2(p.y - Y, BETA * (X - p.x));
    val zeta = math.atan2(-p.vz, p.z);
    val i = (p.z / math.cos(zeta)).abs;
    GCCoord(X, Y, e, phi, i, zeta, p.rad)
  }
}
    
