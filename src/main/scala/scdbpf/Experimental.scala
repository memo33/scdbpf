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

  import ps.tricerato.pureimage._
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

}
