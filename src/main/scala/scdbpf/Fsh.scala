package scdbpf

import compat.{Image, RGBA}
import Fsh._
import java.nio.ByteBuffer
import DbpfUtil._
import io.github.memo33.passera.unsigned._

trait Fsh extends DbpfType {

  val elements: Seq[FshElement]
  val dirId: FshDirectoryId

  def copy(elements: Seq[FshElement] = elements, dirId: FshDirectoryId = dirId): Fsh =
    new FreeFsh(elements, dirId)

  /** Returns the very first image of this FSH container file (for convenience); note that it
    * may contain several more images, see [[elements]].
    */
  def image: Image[RGBA] = elements.head.images.head
}

object Fsh extends DbpfTypeCompanion[Fsh] {

  implicit val converter = new Converter[DbpfType, Fsh] {
    def apply(from: DbpfType): Fsh = {
      try {
        new BufferedFsh(from.dataView)
      } catch {
        case e @ (_: NoSuchElementException
                 |_: java.nio.BufferUnderflowException
                 |_: IllegalArgumentException
                 |_: IndexOutOfBoundsException) =>
          throw new DbpfDecodeFailedException(e.toString, e)
      }
    }
  }

  def apply(elements: Seq[FshElement], dirId: FshDirectoryId = FshDirectoryId.G264): Fsh =
    new FreeFsh(elements, dirId)

  class FshElement(
    val images: Iterable[Image[RGBA]],
    val format: FshFormat,
    val label: Option[String] = None) {

    private val xCenter = 0
    private val yCenter = 0
    private val xOffset = 0
    private val yOffset = 0

    private[Fsh] lazy val binarySize: Int = {
      val sum = images.foldLeft(0x10)((sum, img) => sum + format.dataLength(img.width, img.height))
      label.fold(sum)(_.length + 4 + sum)
    }

    private[Fsh] def encode(buf: ByteBuffer): Unit = {
      // put header
      buf.putInt(binarySize << 8 | format.code & 0xFF) //BYTE - format code, UINT24 - length
      buf.putShort(images.head.width.toShort)        // width
      buf.putShort(images.head.height.toShort)       // height
      buf.putShort(0)
      buf.putShort(0)
      buf.putShort(0)
      buf.putShort((images.size - 1 << 12).toShort)        // mipmaps
      // put images
      for (img <- images) {
        format.encode(buf, img)
      }
      for (s <- label) {
        buf.putInt(0x70)
        buf.put(s.getBytes(asciiEncoding))
      }
    }
  }

  type FshDirectoryId = FshDirectoryId.Val
  object FshDirectoryId extends Enumeration {

    class Val private[FshDirectoryId] (name: String) extends super.Val {
      val code: Int = MagicNumber.fromString(name) // using super.toString causes initialization issues
    }
    /** Building Textures */
    val G354 = new Val("G354")
    /** Network Textures, Sim Textures, Sim heads, Sim animations, Trees, props, Base textures, Misc colors */
    val G264 = new Val("G264")
    /** 3d Animation textures (e.g. the green rotating diamond in loteditor.dat) */
    val G266 = new Val("G266")
    /** Dispatch marker textures */
    val G290 = new Val("G290")
    /** Small Sim texture, Network Transport Model Textures (trains, etc.) */
    val G315 = new Val("G315")
    /** UI Editor textures */
    val GIMX = new Val("GIMX")
    /** BAT gen texture maps */
    val G344 = new Val("G344")
    /** unknown */
    val G231 = new Val("G231")
    /** unknown */
    val G341 = new Val("G341")
    /** unknown */
    val G349 = new Val("G349")
    /** unknown */
    val G352 = new Val("G352")
    /** unknown */
    val G357 = new Val("G357")

    private[Fsh] def withId(id: Int): FshDirectoryId = {
      values.find(_.asInstanceOf[Val].code == id) match {
        case Some(dirId) => dirId.asInstanceOf[Val]
        case None => throw new DbpfDecodeFailedException(f"Unknown FSH directory id 0x$id%08X")
      }
    }
  }

  type FshFormat = FshFormat.Val
  object FshFormat extends Enumeration {

    class Val private[FshFormat] (name: String, val code: Byte) extends super.Val {

      private[Fsh] def dataLength(width: Int, height: Int): Int = this match {
        case Dxt1 =>
          assert(width % 4 == 0 && height % 4 == 0)
          width * height / 2
        case Dxt3 =>
          assert(width % 4 == 0 && height % 4 == 0)
          width * height
        case A0R8G8B8 => 3 * width * height
        case A8R8G8B8 => 4 * width * height
        case A1R5G5B5 => 2 * width * height
        case A0R5G6B5 => 2 * width * height
        case A4R4G4B4 => 2 * width * height
      }

      private[Fsh] def decode(array: Array[Byte], buf: ByteBuffer, offset: Int, width: Int, height: Int): Image[RGBA] = this match {
        case A8R8G8B8 => new Int32Image(buf, offset, width, height)
        case A0R8G8B8 => new Int24Image(buf, offset, width, height)
        case A1R5G5B5 => new ShortImage(buf, offset, width, height)(conversions.short1555toRGBA)
        case A0R5G6B5 => new ShortImage(buf, offset, width, height)(conversions.short0565toRGBA)
        case A4R4G4B4 => new ShortImage(buf, offset, width, height)(conversions.short4444toRGBA)
        case _/*Dxt*/ => DxtDecoding.decode(this, array, offset, width, height)
      }

      private[Fsh] def encode(buf: ByteBuffer, img: Image[RGBA]): Unit = this match {
        case A8R8G8B8 =>
          for (y <- 0 until img.height; x <- 0 until img.width)
            buf.putInt(conversions.rgbaToARGB(img(x, y)))
        case A0R8G8B8 =>
          for (y <- 0 until img.height; x <- 0 until img.width) {
            val rgb = img(x, y)
            buf.put(rgb.blue); buf.put(rgb.green); buf.put(rgb.red)
          }
        case A1R5G5B5 =>
          for (y <- 0 until img.height; x <- 0 until img.width)
            buf.putShort(conversions.rgbaToShort1555(img(x, y)))
        case A0R5G6B5 =>
          for (y <- 0 until img.height; x <- 0 until img.width)
            buf.putShort(conversions.rgbaToShort0565(img(x, y)))
        case A4R4G4B4 =>
          for (y <- 0 until img.height; x <- 0 until img.width)
            buf.putShort(conversions.rgbaToShort4444(img(x, y)))
        case Dxt1 =>
          import io.github.memo33.jsquish.Squish._
          val arr = compressImage(imageToByteArray(img), img.width, img.height, null, CompressionType.DXT1, CompressionMethod.CLUSTER_FIT)
          buf.put(arr)
        case Dxt3 =>
          import io.github.memo33.jsquish.Squish._
          val arr = compressImage(imageToByteArray(img), img.width, img.height, null, CompressionType.DXT3, CompressionMethod.CLUSTER_FIT)
          buf.put(arr)
      }

      private def imageToByteArray(img: Image[RGBA]): Array[Byte] = {
        val data = new Array[Byte](img.width * img.height * 4)
        for (x <- 0 until img.width; y <- 0 until img.height) {
          val i = 4 * (x + y * img.width)
          data(i    ) = img(x, y).red
          data(i + 1) = img(x, y).green
          data(i + 2) = img(x, y).blue
          data(i + 3) = img(x, y).alpha
        }
        data
      }
    }

    val Dxt3 = new Val("DXT3 compressed", 0x61)
    val Dxt1 = new Val("DXT1 compressed", 0x60)
    val A8R8G8B8 = new Val("A8R8G8B8 32bit", 0x7D)
    val A0R8G8B8 = new Val("A0R8G8B8 24bit", 0x7F)
    val A1R5G5B5 = new Val("A1R5G5B5 16bit", 0x7E)
    val A0R5G6B5 = new Val("A0R5G6B5 16bit", 0x78)
    val A4R4G4B4 = new Val("A4R4G4B4 16bit", 0x6D)
    //8-bit indexed with palette?

    private[Fsh] def withId(id: Int): Val = id match {
      case 0x61 => Dxt3
      case 0x60 => Dxt1
      case 0x7D => A8R8G8B8
      case 0x7F => A0R8G8B8
      case 0x7E => A1R5G5B5
      case 0x78 => A0R5G6B5
      case 0x6D => A4R4G4B4
      case _ => throw new DbpfDecodeFailedException(f"Unsupported FSH format 0x$id%02X")
    }
  }

  private[scdbpf] object conversions {

    // sup and newSup are number of bits, essentially
    private def interpolate(sup: Int, newSup: Int, v: Int): Int = {
      if (newSup <= sup)
        v >>> (sup - newSup)
      else if (sup == 0) // e.g. we want opacity, not transparency, if no alpha channel
        (1 << newSup) - 1
      else {
        val v2 = v << (newSup - sup)
        v2 + (v2 >>> sup)
      }
    }

    private def toRGBA(a: Int, r: Int, g: Int, b: Int)(i: Int): RGBA = RGBA(
      interpolate(a, 8, i >>> (r+g+b) & (1 << a)-1) << 24 | // alpha
      interpolate(b, 8, i             & (1 << b)-1) << 16 | // blue
      interpolate(g, 8, i >>> (    b) & (1 << g)-1) <<  8 | // green
      interpolate(r, 8, i >>> (  g+b) & (1 << r)-1)         // red
    )

    private def fromRGBA(a: Int, r: Int, g: Int, b: Int)(p: RGBA): Int = {
      interpolate(8, a, p.alpha & 0xFF) << (r+g+b) | // alpha
      interpolate(8, r, p.red   & 0xFF) <<   (g+b) | // red
      interpolate(8, g, p.green & 0xFF) <<      b  | // green
      interpolate(8, b, p.blue  & 0xFF)              // blue
    }

    val short1555toRGBA = { s: Short => s & 0xffff } andThen toRGBA(1,5,5,5) _
    val short0565toRGBA = { s: Short => s & 0xffff } andThen toRGBA(0,5,6,5) _
    val short4444toRGBA = { s: Short => s & 0xffff } andThen toRGBA(4,4,4,4) _
    val argbToRGBA = toRGBA(8,8,8,8) _

    val rgbaToShort1555 = fromRGBA(1,5,5,5) _ andThen (_.toShort)
    val rgbaToShort0565 = fromRGBA(0,5,6,5) _ andThen (_.toShort)
    val rgbaToShort4444 = fromRGBA(4,4,4,4) _ andThen (_.toShort)
    val rgbaToARGB = fromRGBA(8,8,8,8) _

    def rgbaFromChannels(r: Int, g: Int, b: Int, a: Int): RGBA = {
      RGBA(
         (a & 0xFF) << 24 | // alpha
         (b & 0xFF) << 16 | // blue
         (g & 0xFF) <<  8 | // green
          r & 0xFF          // red
      )
    }
  }

  private class Int32Image(buf: ByteBuffer, offset: Int, val width: Int, val height: Int) extends Image[RGBA] {
    def apply(x: Int, y: Int): RGBA =
      conversions.argbToRGBA(buf.getInt(offset + 4 * (x + y * width)))
  }

  private class Int24Image(buf: ByteBuffer, offset: Int, val width: Int, val height: Int) extends Image[RGBA] {
    def apply(x: Int, y: Int): RGBA = {
      val rgb = getInt24(buf, offset + 3 * (x + y * width))
      conversions.argbToRGBA(rgb | 0xFF000000)
    }
  }

  private class ShortImage(buf: ByteBuffer, offset: Int, val width: Int, val height: Int)(convert: Short => RGBA) extends Image[RGBA] {
    def apply(x: Int, y: Int): RGBA =
      convert(buf.getShort(offset + 2 * (x + y * width)))
  }

  private class BufferedFsh(arr: Array[Byte]) extends RawType(arr) with Fsh {
    val (elements, dirId) = {
      val buf = wrapLEBB(data)
      // fsh header
      val sig = buf.getInt()
      if (sig != MagicNumber.SHPI) {
        throw new DbpfDecodeFailedException(f"Unknown filetype 0x$sig%08X, expected SHPI")
      }
      val size = buf.getInt()
      val numEntries = buf.getInt()
      val dirId = FshDirectoryId.withId(buf.getInt())
      val dirEntries = for (i <- 1 to numEntries) yield (buf.getInt(), buf.getInt())
      val blockStarts = dirEntries.map(_._2)

      // decode each fsh element
      val elems = for ((offset, end) <- blockStarts.zipAll(blockStarts.tail, 0, arr.length)) yield {
        buf.position(offset)
        // element header
        val record = buf.get()
        val blockSize = getInt24(buf)
        val width = UShort(buf.getShort()).toInt
        val height = UShort(buf.getShort()).toInt
        val xCenter = UShort(buf.getShort()).toInt
        val yCenter = UShort(buf.getShort()).toInt
        val xOffsetTmp = buf.getShort()
        val yOffsetTmp = buf.getShort()
        val xOffset = xOffsetTmp & 0x0FFF
        val yOffset = yOffsetTmp & 0x0FFF
        val numMips = if (blockSize == 0) 0 else yOffsetTmp >>> 12 & 0x000F

        val format = FshFormat.withId(record & 0x7F)

        // decode element images
        var pos = offset + 0x10
        val images = for {
          i <- 0 to numMips
          w = width >>> i
          h = height >>> i
          if format != FshFormat.Dxt1 && format != FshFormat.Dxt3 || w % 4 == 0 && h % 4 == 0
        } yield {
          val img = format.decode(data, buf, pos, w, h)
          pos += format.dataLength(w, h)
          img
        }
        // TODO check next byte (palette code or string label)
        val attachment: Option[Array[Byte]] =
          if (blockSize > 0 && offset + blockSize < end)
            Some(data.slice(offset + blockSize, end))
          else None
        val label = for (a <- attachment if a.length >= 4 && a(0) == 0x70) yield {
          val stop = a.indexOf(0, 4)
          new String(a, 4, - 4 + (if (stop != -1) stop else a.length), asciiEncoding)
        }
//        if (images.size != 1 + numMips) {
////          assert(attachment.isDefined, s"pos $pos offset $offset blockSize $blockSize")
////          val arr = attachment.get map (b => f"$b%02X")
//          debug("warning: 2x2 and 1x1 mipmaps have been dropped in " + entry.tgi)
////          debug(" format " + format +
////            " attachment: " + arr.mkString(" ") + " ascii: " + (new String(attachment.get, "US-ASCII")))
//        }
        new FshElement(images, format, label)
      }
      (elems.toSeq, dirId)
    }
  }

  private val BMP1: Int = MagicNumber.fromString("BMP1")

  private class FreeFsh(val elements: Seq[FshElement], val dirId: FshDirectoryId) extends Fsh {

    protected lazy val data = {
      val elemLengths = elements map (_.binarySize)
      val fileLength = 0x10 + 8 * elements.size + elemLengths.sum
      val buf = allocLEBB(fileLength)
      buf.putInt(MagicNumber.SHPI)
      buf.putInt(fileLength)
      buf.putInt(elements.size)
      buf.putInt(dirId.code)

      // put directory
      elemLengths.foldLeft(0x10 + 8 * elements.size){ (pos, len) =>
        buf.putInt(BMP1)
        buf.putInt(pos)
        pos + len
      }

      // put elements
      for (e <- elements) {
        e.encode(buf)
      }
      buf.array()
    }
  }
}
