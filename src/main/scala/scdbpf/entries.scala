package scdbpf

import rapture.io._
import resource._
import DbpfUtil._
import passera.unsigned._
import java.io.{RandomAccessFile, EOFException}

/** The base trait for entries of a DBPF file.
  *
  * @define COPY Creates a new entry with a different `tgi`.
  * @define EXCEPTIONHANDLER
  * An `ExceptionHandler` needs to be brought into scope via imports (either
  * `strategy.throwExceptions` or `strategy.captureExceptions` from the
  * `rapture.io` package).
  */
trait DbpfEntry {

  /** the TGI of this entry */
  def tgi: Tgi

  /** the byte input that provides the raw byte data that gets written to DBPF
    * files
    */
  def input(): Input[Byte]

  /** Converts this entry to a `BufferedEntry`.
    *
    * $EXCEPTIONHANDLER
    */
  def toBufferedEntry(implicit eh: ExceptionHandler): eh.![BufferedEntry[DbpfType], DbpfIoException] = eh wrap {
    val arr: Array[Byte] = managed(this.input()) acquireAndGet (DbpfUtil.slurpBytes(_))
    BufferedEntry(this.tgi, RawType(arr), DbpfPackager.isCompressed(arr))
  }

  /** Converts this entry to a `RawEntry`.
    *
    * $EXCEPTIONHANDLER
    */
  def toRawEntry(implicit eh: ExceptionHandler): eh.![RawEntry, DbpfIoException] = eh wrap {
    val arr: Array[Byte] = managed(this.input()) acquireAndGet (DbpfUtil.slurpBytes(_))
    new RawEntry(this.tgi, arr)
  }
}

/** A buffered entry whose data is held in memory in ''uncompressed'' form.
  * The actual data will be held by the `content` type. As such, this entry
  * will be immutable, if its `content` is immutable.
  *
  * @see [[RawEntry]]
  * @see [[StreamedEntry]]
  *
  * @tparam A the type of the content
  * @param tgi the TGI of this entry
  * @param content the actual content of this entry
  * @param compressed `true`, if the [[input]] ''should try'' to return the data
  * in compressed form. If compression fails (if the compressed data would be
  * larger than the compressed one, for instance), the data may be uncompressed
  * nevertheless.
  */
final case class BufferedEntry[+A <: DbpfType](val tgi: Tgi, val content: A, val compressed: Boolean)
  extends DbpfEntry {

  /** @inheritdoc
    * It is tried to respect the `compressed` flag, as long as this reduces the
    * data size.
    */
  def input(): Input[Byte] = {
    val data = content.dataView
    val outData = if (compressed) {
      DbpfPackager.compress(data)
    } else {
      assert(!DbpfPackager.isCompressed(data))
      data
    }
    new ByteArrayInput(outData)
  }

  /** Creates a new `BufferedEntry`, with this entry's `content` converted to `B`.
    * Usually this delegates to the `convert` method of `content`, but in some
    * cases this is not possible, like for [[Exemplar]], which requires the
    * `tgi` for conversion.
    */
  def convert[B <: DbpfType](implicit eh: ExceptionHandler,
      conv: Converter[BufferedEntry[A], BufferedEntry[B]]): eh.![BufferedEntry[B], DbpfDecodeFailedException] = {
    eh wrap conv(this)
  }

  override def toBufferedEntry(implicit eh: ExceptionHandler): eh.![BufferedEntry[DbpfType], DbpfIoException] =
    eh wrap this

  override def toRawEntry(implicit eh: ExceptionHandler): eh.![RawEntry, DbpfIoException] =
    eh wrap new RawEntry(tgi, if (compressed) DbpfPackager.compress(content.dataView) else content.dataView)
}

/** A buffered entry whose raw byte data is held in an array, that is, the exact
  * data that would be written to a file (compressed or uncompressed).
  *
  * This differs from a [[BufferedEntry]] in that the latter always holds the
  * ''uncompressed'' data. Thus, a `BufferedEntry` is suited for access and
  * modification of the content, but a `RawEntry` is usually suitable for just
  * holding the data in memory, as creating the [[input]] does not require an
  * intermediate compression. If the data is not compressed, this distinction
  * is not as important.
  *
  * Instances of this class are immutable, if the data it was created from was.
  * Note that conversion between `RawEntries` and `BufferedEntries`
  * references the same backing array if possible. If created from a
  * [[StreamedEntry]], a new data array will be created, hence, the
  * entry will be immutable.
  *
  * @see [[BufferedEntry]]
  * @see [[StreamedEntry]]
  */
class RawEntry(val tgi: Tgi, data: Array[Byte]) extends DbpfEntry {

  def input(): Input[Byte] = new ByteArrayInput(data)

  def compressed: Boolean = DbpfPackager.isCompressed(data)

  /** $COPY */
  def copy(tgi: Tgi): RawEntry = new RawEntry(tgi, data)

  override def toRawEntry(implicit eh: ExceptionHandler): eh.![RawEntry, DbpfIoException] = eh wrap this

  override def toBufferedEntry(implicit eh: ExceptionHandler): eh.![BufferedEntry[DbpfType], DbpfIoException] =
    eh wrap BufferedEntry(tgi, RawType(data), compressed)
}

/** A lightweight entry that reads its data as stream from a file without
  * holding all the data constantly in memory.
  *
  * Usually, it is not necessary to load all the entries into memory,
  * especially when processing large files, so instances of this class can be
  * directly read from and written to files. Note that this is sensitive to
  * changes of the source file: If the source file has been modified since this
  * entry had been created, a [[DbpfStreamOutOfDateException]] will be thrown
  * upon calling [[input]] so as to preclude malformed data.
  *
  * @see [[RawEntry]]
  * @see [[BufferedEntry]]
  *
  * @param size the binary size of this entry
  */
final class StreamedEntry private[scdbpf] (file: JFile, val tgi: Tgi, offset: UInt, val size: UInt, dateModified: UInt) extends DbpfEntry {

  /** $COPY */
  def copy(tgi: Tgi): StreamedEntry = new StreamedEntry(file, tgi, offset, size, dateModified)

  /** @inheritdoc
    * @throws DbpfStreamOutOfDateException if the underlying file has changed since
    * this entry was created.
    */
  def input: Input[Byte] = new StreamedDbpfInput()

  private class StreamedDbpfInput() extends DbpfFile.AbstractByteInput {
    var remaining = StreamedEntry.this.size.toInt
    val raf = new RandomAccessFile(file, "r");
    {
      val date = DbpfUtil.readDbpfDateModified(raf)
      if (dateModified != date) {
        throw new DbpfStreamOutOfDateException(
          f"Entry $tgi from file $file was created based on file date " +
          f"0x${dateModified.toInt}%08X, but current file date is 0x${date.toInt}%08X")
      }
    }
    raf.seek(offset.toLong)

    def close(): Unit = raf.close()

    def ready(): Boolean = true

    override def readBlock(array: Array[Byte], offset: Int, length: Int): Int = {
      assert(remaining >= 0)
      if (remaining <= 0) -1
      else {
        val len =
          if (length == -1) array.length - offset
          else length
        val count = raf.read(array, offset, remaining min len)
        if (count == -1) throw new EOFException("Entry data would reach beyond end of file")
        else {
          remaining -= count
          count
        }
      }
    }
  }
}
