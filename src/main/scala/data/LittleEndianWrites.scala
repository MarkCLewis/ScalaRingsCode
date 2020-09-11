package data

import java.io.DataOutputStream

object LittleEndianWrites {
  def writeInt(dos: DataOutputStream, i: Int) = dos.writeInt(Integer.reverseBytes(i))

  def writeDouble(dos: DataOutputStream,  x: Double) = dos.writeLong(java.lang.Long.reverseBytes(java.lang.Double.doubleToRawLongBits(x)))
}