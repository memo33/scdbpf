package scdbpf

import java.nio.{ByteBuffer, ByteOrder}
import io.github.memo33.passera.unsigned._

object DbpfUtil {

  // all the magic numbers are encoded little endian
  object MagicNumber {

    private[scdbpf] def fromString(s: String): Int = {
      assert(s.length <= 4)
      s.toSeq.map(_.toByte).map(_ & 0xff).reduceRight(_ + 0x100 * _)
    }

    val DBPF: Int = fromString("DBPF")

    val QFS: Short = 0xFB10.toShort

    val SHPI: Int = fromString("SHPI")

    val `3DMD`: Int = fromString("3DMD")

    val EQZB: Int = fromString("EQZB")
    val EQZT: Int = fromString("EQZT")
    val CQZB: Int = fromString("CQZB")
    val CQZT: Int = fromString("CQZT")
    /* exemplar/cohort version 1 */
    val `1###`: Int = fromString("1###")
  }

  private[scdbpf] def allocLEBB(size: Int) = {
    val buf = ByteBuffer.allocate(size)
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf
  }

  private[scdbpf] def wrapLEBB(array: Array[Byte]): ByteBuffer = {
    val buf = ByteBuffer.wrap(array)
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf
  }

  private[scdbpf] def getInt24(buf: ByteBuffer): Int = {
    assert(buf.order() == ByteOrder.LITTLE_ENDIAN)
    buf.get() & 0xFF | (buf.get() & 0xFF) << 8 | (buf.get() & 0xFF) << 16
  }

  private[scdbpf] def getInt24(buf: ByteBuffer, pos: Int): Int = {
    assert(buf.order() == ByteOrder.LITTLE_ENDIAN)
    buf.get(pos) & 0xFF | (buf.get(pos + 1) & 0xFF) << 8 | (buf.get(pos + 2) & 0xFF) << 16
  }

//  private[scdbpf] def putInt24(buf: ByteBuffer, i: Int): Int = {
//    assert(buf.order() == ByteOrder.LITTLE_ENDIAN)
//    buf.put(i.toByte)
//    buf.put((i >> 8).toByte)
//    buf.put((i >> 16).toByte)
//  }

  private[scdbpf] def toHex(i: Int): String = {
    val s = i.toHexString
    "0x" + "0" * (8 - s.length) + s.toUpperCase
  }

  import compat.{Input, ByteOutput}
  import java.io.{ByteArrayOutputStream}
  private[scdbpf] def slurpBytes(input: Input[Byte]): Array[Byte] = {
    // import strategy.throwExceptions
    // input.slurp[Byte]()
    val baos = new ByteArrayOutputStream
    val output = new ByteOutput(baos)
    input.pumpTo(output)
    baos.toByteArray
  }

  private[scdbpf] val asciiEncoding = java.nio.charset.Charset.forName("US-ASCII")

  private val dateModifiedOffset = 28
  private[scdbpf] def readDbpfDateModified(raf: java.io.RandomAccessFile): UInt = {
    raf.seek(dateModifiedOffset)
    val i = raf.readInt()
    // swap endianess
    UInt((i << 24) | (i << 8) & 0x00FF0000 | (i >>> 8) & 0x0000FF00 | (i >>> 24))
  }

  type RotFlip = RotFlip.Val
  object RotFlip extends Enumeration {

    class Val private[RotFlip] (val rot: Int, val flip: Int) extends super.Val {

      /** returns [[flip]] as Boolean */
      def flipped: Boolean = flip == 1

      /** Group multiplication from *left*, but written from *right*.
        */
      def * (that: RotFlip): RotFlip = {
        val r = (this.rot + (if (this.flip == 1) -1 else 1) * that.rot) & 0x3
        val f = if (this.flip != that.flip) 1 else 0
        withRotFlip(r, f)
      }

      /** Group division from *left*, but written from *right* (multiplication
        * with right operand inverted).
        */
      def / (that: RotFlip): RotFlip = {
        val r = (this.rot + (if (this.flip != that.flip) 1 else -1) * that.rot) & 0x3
        val f = if (this.flip != that.flip) 1 else 0
        withRotFlip(r, f)
      }

      def *: [A, B](a: A)(implicit dih: Dihedral[A, B], ev: Negatable[B]): A = dih(a, this)

      def /: [A, B](a: A)(implicit dih: Dihedral[A, B], ev: Negatable[B]): A = dih(a, R0F0 / this)

      override def toString = "(" + rot + "," + flip + ")"
    }

    val R0F0 = new Val(0,0)
    val R1F0 = new Val(1,0)
    val R2F0 = new Val(2,0)
    val R3F0 = new Val(3,0)
    val R0F1 = new Val(0,1)
    val R1F1 = new Val(1,1)
    val R2F1 = new Val(2,1)
    val R3F1 = new Val(3,1)

    private def withRotFlip(rot: Int, flip: Int): RotFlip = {
      RotFlip(rot + flip * 4).asInstanceOf[RotFlip]
    }

    def apply(rot: Int, flip: Int): RotFlip = {
      require(rot >= 0 && rot < 4 && (flip == 0 || flip == 1))
      withRotFlip(rot, flip)
    }
  }

  trait Negatable[B] {
    def negate(b: B): B
  }

  trait Dihedral[A, B] {
    /** x component oriented in "right"-direction */
    def x(a: A): B
    /** y component oriented in "up"-direction (or rather "rear") */
    def y(a: A): B
    /** builds a new A with x and y components replaced */
    def build(from: A, x: B, y: B): A

    import RotFlip._
    def apply(a: A, rf: RotFlip)(implicit ev: Negatable[B]): A = rf match {
      case R0F0 => a                                          // +x, +y
      case R1F0 => build(a,           y(a) , ev.negate(x(a))) // +y, -x
      case R2F0 => build(a, ev.negate(x(a)), ev.negate(y(a))) // -x, -y
      case R3F0 => build(a, ev.negate(y(a)),           x(a) ) // -y, +x
      case R0F1 => build(a, ev.negate(x(a)),           y(a) ) // -x, +y
      case R1F1 => build(a, ev.negate(y(a)), ev.negate(x(a))) // -y, -x
      case R2F1 => build(a,           x(a) , ev.negate(y(a))) // +x, -y
      case R3F1 => build(a,           y(a) ,           x(a) ) // +y, +x
    }
  }

  implicit def numericToNegatable[B : Numeric] = new Negatable[B] { def negate(b: B) = implicitly[Numeric[B]].negate(b) }

  object Dihedral {

    import S3d.Vert
    implicit object VertIsDihedral extends Dihedral[Vert, Float] {
      def x(a: Vert): Float = a.x
      def y(a: Vert): Float = -a.z
      def build(from: Vert, x: Float, y: Float): Vert = from.copy(x = x, z = -y)
    }

    import Sc4Path.Coord
    implicit object CoordIsDihedral extends Dihedral[Coord, Float] {
      def x(a: Coord): Float = a._1
      def y(a: Coord): Float = a._2
      def build(from: Coord, x: Float, y: Float): Coord = (x, y, from._3)
    }

    import Sc4Path.Cardinal
    implicit object CardinalIsDihedral extends Dihedral[Cardinal, Int] {
      import Sc4Path.Cardinal._
      def x(a: Cardinal): Int = if (a == East) 1 else if (a == West) -1 else 0
      def y(a: Cardinal): Int = if (a == North) 1 else if (a == South) -1 else 0
      def build(from: Cardinal, x: Int, y: Int): Cardinal = (x, y) match {
        case (-1, 0) => West
        case ( 0, 1) => North
        case ( 1, 0) => East
        case ( 0,-1) => South
        case ( 0, 0) => Special
      }
    }
  }

//  trait QfsCompressed { this: DbpfEntry =>
//
//    /** determines whether input() returns the data in compressed or uncompressed format */
//    def compressed: Boolean
//
//    /** the raw byte data of the entry, either compressed or uncompressed */
//    protected def data(): Array[Byte]
//
//    /** takes the data() array and transforms it as input; no matter whether data()
//      * is compressed, the input() will usually be compressed iff compressed() is true
//      * (with the following exception: If the uncompressed data is shorter than the
//      * compressed data, everything is ignored and it will be uncompressed).
//      */
//    final def input(): Input[Byte] = {
//      val data = this.data()
//      val dataCompressed = DbpfPackager.isCompressed(data)
//      val outData = if (dataCompressed == compressed) {
//        data
//      } else if (dataCompressed) {
//        DbpfPackager.decompress(data)
//      } else {
//        try {
//          DbpfPackager.compress(data)
//        } catch {
//          case _: IndexOutOfBoundsException =>
//            println(s"warning: failed to compress ${tgi}, used uncompressed data instead")
//            data
//        }
//      }
//      new ByteArrayInput(outData)
//    }
//  }

  trait Converter[-A, +B] {
    def apply(from: A): B
  }

}
