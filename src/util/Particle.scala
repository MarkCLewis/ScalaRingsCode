package util

case class Particle(x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double, rad: Double) {

}

case class GCCoord(X: Double, Y: Double, e: Double, phi: Double, i: Double, zeta: Double) {

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
    GCCoord(X, Y, e, phi, i, zeta)
  }
}
    
