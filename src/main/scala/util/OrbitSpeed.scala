package util

object OrbitSpeed {
  def apply(centralMass: Double, r0: Double, v: Double): Double = {
    v * conversionRation(centralMass, r0)
  }

  /**
   * Expects values in kg and km. Uses v0 = sqrt(Gm/r). Gives ratio from simulation units to m/s.
   * v0 is 1.0 in simulation units (R0/T). 
   */
  def conversionRation(centralMass: Double, r0: Double): Double = {
    math.sqrt(6.67e-11*centralMass/(r0*1000))  // v0 in m/s
  }
}