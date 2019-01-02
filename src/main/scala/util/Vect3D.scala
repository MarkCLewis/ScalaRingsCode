package util

case class Vect3D(x:Double, y:Double, z:Double) {
  def +(v:Vect3D) = new Vect3D(x+v.x, y+v.y, z+v.z)
  def -(v:Vect3D) = new Vect3D(x-v.x, y-v.y, z-v.z)
  def *(c:Double) = new Vect3D(x*c, y*c, z*c)
  def /(c:Double) = new Vect3D(x/c, y/c, z/c)
  def dot(v:Vect3D) = x*v.x + y*v.y + z*v.z
}