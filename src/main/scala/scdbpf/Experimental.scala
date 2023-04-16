package scdbpf

/** Provides a few experimental functions that are useful, but which may still
  * be changed in the future.
  */
object Experimental {

  private def hasDbpfExtension(file: JFile): Boolean = {
    val idx = file.getName.lastIndexOf('.')
    if (idx == -1) {
      false
    } else {
      val ext = file.getName.substring(idx + 1).toLowerCase
      ext == "dat" || ext == "sc4model" || ext == "sc4lot" || ext == "sc4desc"
    }
  }

  private val dbpfFileFilter = new java.io.FileFilter {
    def accept(f: JFile) = f.isDirectory || hasDbpfExtension(f)
  }

  /** Iterates over all the files in a directory tree that have a DBPF extension,
    * that is one of '.dat', 'sc4model', 'sc4lot', 'sc4desc'.
    */
  def dbpfFileTreeIterator(file: JFile): Iterator[JFile] = {
    if (file.isDirectory) {
      val children = file.listFiles(dbpfFileFilter).iterator
      children.flatMap(dbpfFileTreeIterator(_))
    } else {
      Iterator(file)
    }
  }

  import ps.tricerato.pureimage.{Image, RGBA}
  import java.awt.image.BufferedImage
  /** Creates a new `BufferedImage` from an `Image`.
    */
  def imageToBufferedImage(img: Image[RGBA]): BufferedImage = {
    val w = img.width
    val h = img.height
    val arr = new Array[Int](img.width * img.height)
    for (i <- 0 until h; j <- 0 until w) {
      arr(i * w + j) = Fsh.conversions.rgbaToARGB(img(j, i))
    }
    val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    bi.setRGB(0, 0, w, h, arr, 0, w)
    bi
  }

  import scala.language.implicitConversions
  /** Wraps a `BufferedImage` as an `Image`.
    */
  implicit def bufferedImageAsImage(img: BufferedImage): Image[RGBA] = new Image[RGBA] {
    def height = img.getHeight
    def width = img.getWidth
    def apply(x: Int, y: Int): RGBA = Fsh.conversions.argbToRGBA(img.getRGB(x, y))
  }

  object PreviewEffect extends ((Int, String) => DbpfType) {
    private val TemplateLength = 0x04F0
    private lazy val templateData = {
      import compat.ByteInput, resource._
      managed(new ByteInput(getClass.getResourceAsStream("/preview_effect_template.eff"))) acquireAndGet { in =>
        val arr = DbpfUtil.slurpBytes(in)
        assert(arr.length == TemplateLength)
        arr
      }
    }
    private val IdIndex = Seq(0x00c8, 0x027a)
    private val LengthIndex = Seq(0x03ae, 0x042d, 0x04a0, 0x04a8)

    /** Constructs a puzzle piece preview effect directory file.
      * @param iid the ID of the preview model
      * @param refName the name by which the effect is referenced in RUL0; it
      * will be converted to lower-case and prefixed with 'preview_' if not
      * already.
      */
    def apply(iid: Int, refName: String) = new DbpfType {
      private val name = {
        val low = refName.toLowerCase
        if (low.startsWith("preview_")) low else "preview_" + low
      }
      protected lazy val data = {
        val buf = DbpfUtil.allocLEBB(templateData.length + 4 * name.length + 8)
        def transfer(from: Int, to: Int) = buf.put(templateData, from, to - from)
        val nameBytes = name.getBytes(DbpfUtil.asciiEncoding)
        val nameBytesRed = (name + "_red").getBytes(DbpfUtil.asciiEncoding)
        transfer(0,              IdIndex(0)) putInt iid
        transfer(IdIndex(0) + 4, IdIndex(1)) putInt iid
        transfer(IdIndex(1) + 4,     LengthIndex(0)) putInt name.length       put nameBytes
        transfer(LengthIndex(0) + 4, LengthIndex(1)) putInt (name.length + 4) put nameBytesRed
        transfer(LengthIndex(1) + 4, LengthIndex(2)) putInt name.length       put nameBytes
        transfer(LengthIndex(2) + 4, LengthIndex(3)) putInt (name.length + 4) put nameBytesRed
        transfer(LengthIndex(3) + 4, templateData.length)
        assert(buf.remaining == 0)
        buf.array
      }
    }
  }

}
