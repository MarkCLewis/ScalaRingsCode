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
    case Regex(star, date, b, phiMin, phiMax, rmin, rmax, duration, i0) =>
      Some(MeasurementDetails(star, date, b.toDouble, phiMin.toDouble, phiMax.toDouble, rmin.toDouble, rmax.toDouble, duration.toDouble, i0.toDouble))
    case _ => None
  }

  def fromCSV(line: String, delim: String = "\\s*,\\s*"): Option[MeasurementDetails] = {
    try {
      val p = line.split(delim)
      Some(MeasurementDetails(p(0), p(1), p(2).toDouble, p(3).toDouble, p(4).toDouble, p(5).toDouble, p(6).toDouble, p(7).toDouble, p(8).toDouble))
    } catch {
      case ex: NumberFormatException => None
      case ex: ArrayIndexOutOfBoundsException => None
    }
  }

  val baseStars = Vector(
      MeasurementDetails("α Vir (8) I", "2005-141", 17.2, 116.1, 150.2, 118979, 141954, 2546, 479000),
      MeasurementDetails("α Vir (8) E", "2005-141", 17.2, 116.1, 82.2, 118979, 141704, 2535, 509000),
      MeasurementDetails("α Vir (30) I", "2006-285", 17.2, 266.2, 219.8, 64022, 151545, 4772, 535000),
      MeasurementDetails("α Vir (34) I", "2006-337", 17.2, 282.1, 220.9, 74536, 153654, 4061, 506000),
      MeasurementDetails("α Vir (34) E", "2006-337", 17.2, 282.1, 344.6, 74536, 160059, 4281, 516000),
      MeasurementDetails("α Vir (116) I", "2009-223", 17.2, 245.2, 241.9, 103059, 144566, 3764, 165000),
      MeasurementDetails("α Vir (124) E", "2010-011", 17.2, 121.7, 124.1, 70494, 142431, 6011, 165000),
      MeasurementDetails("α Leo (9) I", "2005-159", 9.5, 68.0, 10.7, 114150, 204718, 6948, 46500),
      MeasurementDetails("α Leo (9) E", "2005-159", 9.5, 68.0, 98.4, 114150, 131539, 2663, 43200),
      MeasurementDetails("γ Peg (36) I", "2006-363", 20.3, 101.6, 156.6, 102296, 178178, 9939, 73000),
      MeasurementDetails("γ Peg (36) E", "2006-363", 20.3, 101.6, 55.7, 102296, 146785, 7172, 70100),
      MeasurementDetails("β Cen (77) I", "2008-202", 66.7, 282.9, 264.4, 73333, 144893, 9481, 583000),
      MeasurementDetails("β Cen (77) E", "2008-203", 66.7, 34.6, 54.4, 73267, 143444, 10191, 604000)
      )
}