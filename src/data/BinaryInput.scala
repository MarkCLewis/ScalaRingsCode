package data

import java.io.DataInput
import java.io.RandomAccessFile
import java.io.InputStream

class BinaryInput(val wrapped: DataInput, littleEndian: Boolean) {

  /**
   * Closes the wrapped file.
   * @throws IOException passes on the exception from the close method.
   */
  def close(): Unit = {
    wrapped match {
      case is: InputStream => is.close()
      case raf: RandomAccessFile => raf.close()
    }
  }

  /**
   * Reads a float in Java binary format.
   * @return The float that was read.
   * @throws IOException This will be thrown if there is a problem reading 4 bytes.
   */
  def readJavaReal4(): Double = {
    wrapped.readFloat();
  }

  /**
   * Reads a double in Java binary format.
   * @return The double that was read.
   * @throws IOException This will be thrown if there is a problem reading 8 bytes.
   */
  def readJavaReal8(): Double = {
    wrapped.readDouble();
  }

  /**
   * This reads in four bytes and tries to convert them to a floating point value.
   * This method references the options for whether you are reading a big-endian
   * or a little-endian file.
   * @return The float that was read.
   * @throws IOException This is thrown if there is a problem reading 4 bytes.
   */
  def readReal4(): Double = {
    java.lang.Float.intBitsToFloat(readInt4());
  }

  /**
   * This reads in eight bytes and tries to convert them to a floating point value.
   * This method references the options for whether you are reading a big-endian
   * or a little-endian file.
   * @return The double that was read.
   * @throws IOException This is thrown if there is a problem reading 8 bytes.
   */
  def readReal8(): Double = {
    java.lang.Double.longBitsToDouble(readInt8());
  }

  /**
   * This reads in a single 4-byte real in XDR format.
   * @return The value that was read.
   * @throws IOException This is thrown if four bytes couldn't be read.
   */
  def readXDR4(): Double = {
    readJavaReal4();
  }

  /**
   * This reads in a single 8-byte real in XDR format.
   * @return The value that was read.
   * @throws IOException This is thrown if eight bytes couldn't be read.
   */
  def readXDR8(): Double = {
    readJavaReal8();
  }

  /**
   * Reads in eight bytes and compiles them to an integer value.  It will change
   * byte order if needed for endianness.
   * @return The integer value that was read.
   * @throws IOException This is thrown if eight bytes couldn't be read.
   */
  def readInt8(): Long = {
    var ret = wrapped.readLong();
    if (littleEndian) {
      ret = java.lang.Long.reverseBytes(ret);
    }
    return ret;
  }

  /**
   * Reads in four bytes and compiles them to an integer value.  It will change
   * byte order if needed for endianness.
   * @return The integer value that was read.
   * @throws IOException This is thrown if four bytes couldn't be read.
   */
  def readInt4(): Int = {
    var ret = wrapped.readInt();
    if (littleEndian) {
      ret = Integer.reverseBytes(ret);
    }
    return ret;
  }

  /**
   * Reads in two bytes and compiles them to an integer value.  It will change
   * byte order if needed for endianness.
   * @return The integer value that was read.
   * @throws IOException This is thrown if two bytes couldn't be read.
   */
  def readInt2(): Int = {
    var ret = wrapped.readShort();
    if (littleEndian) {
      ret = java.lang.Short.reverseBytes(ret);
    }
    return ret;
  }

  /**
   * Reads two bytes and converts them to an int according to XDR format.
   * @return The value read.
   * @throws IOException This is thrown if two bytes could not be read.
   */
  def readIntXDR2(): Int = {
    wrapped.readShort();
  }

  /**
   * Reads four bytes and converts them to an int according to XDR format.
   * @return The value read.
   * @throws IOException This is thrown if four bytes could not be read.
   */
  def readIntXDR4(): Int = {
    wrapped.readInt();
  }

  /**
   * Reads eight bytes and converts them to an int according to XDR format.
   * @return The value read.
   * @throws IOException This is thrown if eight bytes could not be read.
   */
  def readIntXDR8(): Long = {
    wrapped.readLong();
  }

  /**
   * Reads four bytes and converts them to an int according to Java format.
   * @return The value read.
   * @throws IOException This is thrown if four bytes could not be read.
   */
  def readJavaInt4(): Int = {
    wrapped.readInt();
  }

  /**
   * Reads two bytes and converts them to an int according to Java format.
   * @return The value read.
   * @throws IOException This is thrown if two bytes could not be read.
   */
  def readJavaInt2(): Int = {
    wrapped.readShort();
  }

  /**
   * This reads in C-style string.  That is a null terminated array of chars.
   * @return A Java string representing the C-string.
   * @throws IOException This will be thrown if there is an exception before a 0 is read in.
   */
  def readCString(): String = {
    val buf = new StringBuffer();
    var b = wrapped.readByte();
    while (b != 0) {
      buf.append(b.toChar);
      b = wrapped.readByte();
    }
    return buf.toString();
  }

  /**
   * Reads a single byte.
   * @return The byte read.
   * @throws IOException This is thrown if a byte can't be read.
   */
  def readByte(): Byte = {
    wrapped.readByte();
  }

  /**
   * Reads a full array of bytes.
   * @param b The byte to read the values into.
   * @throws IOException This will be thrown if b.length bytes can not be read in.
   */
  def readFully(b: Array[Byte]): Unit = {
    wrapped.readFully(b);
  }
}