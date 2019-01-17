package data

import java.io.DataInputStream

object LittleEndianReads {
  def readInt(dis: DataInputStream) = Integer.reverseBytes(dis.readInt())

  def readDouble(dis: DataInputStream) = java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes(dis.readLong))

}