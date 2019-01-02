package occultations

case class MeasurementDetails(
  star:     String,
  date:     String,
  B:        Double, // Angle between ring plane and line if sight to the star, degrees
  phiMin:   Double, // Angle between radial direction and line of sight dropped to the ring plane, counterclockwise, degrees
  phiMax:   Double,
  rmin:     Double, // km
  rmax:     Double, // km
  duration: Double, // sec
  i0:       Double // Hz
)

object MeasurementDetails {
  val Regex = """MeasurementDetails\((.+),(.+),([\d.]+),([\d.]+),([\d.]+),([\d.]+),([\d.]+),([\d.]+),([\d.]+)\)""".r

  def apply(line: String): Option[MeasurementDetails] = line match {
    case Regex(star, date, b, phMin, phiMax, rmin, rmax, duration, i0) =>
      Some(MeasurementDetails(star, date, b.toDouble, phMin.toDouble, phiMax.toDouble, rmin.toDouble, rmax.toDouble, duration.toDouble, i0.toDouble))
    case _ => None
  }
}