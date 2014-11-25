package scdbpf

import resource._
import rapture.io._
import java.io.{RandomAccessFile, IOException, EOFException, FileOutputStream, SequenceInputStream}
import java.nio.{ByteBuffer, IntBuffer}
import passera.unsigned.UInt
import strategy.throwExceptions
import scala.collection.immutable.{IndexedSeq, Map}

/** A container for `DbpfEntries` that are read from and written to a file.
  * Instances of this class may be obtained via the [[DbpfFile.read]] method.
  * The `write` methods may be used for writing entries back to a file.
  *
  * A `DbpfFile` is immutable, but the entries of this file might not be.
  *
  * @param file the file whose contents this container represents
  * @param header the header of the DBPF file
  * @param entries the entries of this DBPF file
  */
class DbpfFile private (
  val file: JFile,
  val header: DbpfFile.Header,
  val entries: IndexedSeq[StreamedEntry]) {

  /** maps the TGIs of this file to its entries. If the file contains duplicate
    * instances of the same TGI, this map links to the first occurence (the one
    * that would be seen by the game).
    */
  lazy val tgiMap: Map[Tgi, StreamedEntry] =
    entries.reverseMap(e => (e.tgi, e))(scala.collection.breakOut)

  /** Writes a `DbpfFile` back to a file.
    *
    * This method behaves like [[DbpfFile.write]], except that it preserves the
    * creation date of this `DbpfFile` and provides suitable default parameters.
    *
    * @param entries the entries to be written, defaults to [[entries]]
    * @param file the target file, defaults to [[file]]
    * @return a new `DbpfFile` that represents the current state of the `file`
    *
    * @throws DbpfStreamStreamOutOfDateException if the source file has been
    * modified since a StreamedEntry was created
    * @throws FileNotFoundException if the target file is inaccessible
    * @throws IOException in case of other IO errors
    *
    * @see [[DbpfFile.write]]
    */
  def write(
      entries: TraversableOnce[DbpfEntry] = entries,
      file: JFile = file)
        (implicit eh: ExceptionHandler): eh.![DbpfFile, IOException] =
          DbpfFile.write(entries, file, header.dateCreated)(eh)
}

/** Provides factory methods for reading and writing DBPF files.
  */
object DbpfFile {
  import DbpfUtil.allocLEBB

  private val HeaderSize = 0x60 // 96dec
  private val IndexBufferSize = 1024 * 8

  /** Contains the values of the header (first 96 bytes) of a DBPF file. */
  class Header private[DbpfFile] (
    val majorVersion: UInt = UInt(1),
    val minorVersion: UInt = UInt(0),
    val dateCreated: UInt,
    val dateModified: UInt,
    val indexType: UInt = UInt(7),
    val indexEntryCount: UInt,
    val indexOffsetLocation: UInt,
    val indexSize: UInt,
    val holeEntryCount: UInt = UInt(0),
    val holeOffsetLocation: UInt = UInt(0),
    val holeSize: UInt = UInt(0)) {

    if (majorVersion != UInt(1) || minorVersion != UInt(0))
      throw new DbpfFileFormatException(f"Unsupported major.minor version ($majorVersion.$minorVersion), currently only 1.0 is supported")
    if (indexType != UInt(7))
      throw new DbpfFileFormatException(f"Unsupported index type ($indexType), currently only 7 is supported")

    override def toString: String = {
      val values = Seq(majorVersion, minorVersion, dateCreated, dateModified, indexType, indexEntryCount, indexOffsetLocation, indexSize, holeEntryCount, holeOffsetLocation, holeSize)
      val names = Seq("majorVersion", "minorVersion", "dateCreated ", "dateModified", "indexType   ", "indxEntryCnt", "indexOffsLoc", "indexSize   ",  "holeEntryCnt",  "holeOffsLoc ",  "holeSize    ")
      names zip values map (tup => tup._1 + f": 0x${tup._2.toInt}%08X") grouped (4) map (_.mkString(", ")) mkString (f"%n")
    }

    private[DbpfFile] def toArray: Array[Byte] = {
      val buf = allocLEBB(HeaderSize)
      buf.putInt(DbpfUtil.MagicNumber.DBPF)
      buf.putInt(majorVersion.toInt)
      buf.putInt(minorVersion.toInt)
      buf.putInt(0)
      buf.putInt(0)
      buf.putInt(0)
      buf.putInt(dateCreated.toInt)
      buf.putInt(dateModified.toInt)
      buf.putInt(indexType.toInt)
      buf.putInt(indexEntryCount.toInt)
      buf.putInt(indexOffsetLocation.toInt)
      buf.putInt(indexSize.toInt)
      buf.putInt(holeEntryCount.toInt)
      buf.putInt(holeOffsetLocation.toInt)
      buf.putInt(holeSize.toInt)
      buf.array()
    }
  }

  private def Header(buf: IntBuffer): Header = {
    // the int buffer starts after the magic number DBPF
    assert(buf.position() == 0 && buf.capacity() == -1 + DbpfFile.HeaderSize / 4)
    val majorVersion        = UInt(buf.get())
    val minorVersion        = UInt(buf.get())
    buf.get(); buf.get(); buf.get() // skip three empty ints
    val dateCreated         = UInt(buf.get())
    val dateModified        = UInt(buf.get())
    val indexType           = UInt(buf.get())
    val indexEntryCount     = UInt(buf.get())
    val indexOffsetLocation = UInt(buf.get())
    val indexSize           = UInt(buf.get())
    val holeEntryCount      = UInt(buf.get())
    val holeOffsetLocation  = UInt(buf.get())
    val holeSize            = UInt(buf.get())

    new Header(majorVersion, minorVersion, dateCreated, dateModified,
      indexType, indexEntryCount, indexOffsetLocation, indexSize,
      holeEntryCount, holeOffsetLocation, holeSize)
  }

  /** Creates a new `DbpfFile` by the reading the contents from a file.
    *
    * @param file the file to read
    * @return a `DbpfFile` corresponding to the `file`
    *
    * @throws DbpfFileFormatException if magic number is not DBPF, or DBPF format
    *         is not supported (i.e. major.minor version is not 1.0)
    * @throws FileNotFoundException if file does not exist or is inaccessible
    * @throws EOFException if the end of the file is reached unexpectedly
    * @throws IOException in case of other IO errors
    */
  def read(file: JFile)(implicit eh: ExceptionHandler): eh.![DbpfFile, IOException] = {
    eh wrap managed(new RandomAccessFile(file, "r")).acquireAndGet { raf =>
      val buf = allocLEBB(HeaderSize)
      raf.readFully(buf.array(), 0, 4)
      val magic = buf.getInt()
      if (magic != DbpfUtil.MagicNumber.DBPF) {
        throw new DbpfFileFormatException("File is not a DBPF formatted file according to magic number: " + file)
      } else if (file.length > Int.MaxValue.toLong) {
        throw new DbpfIoException("File is too large (larger than 2 GiB): " + file)
      } else {
        // read rest of header
        raf.readFully(buf.array(), 4, HeaderSize - 4)
        val header = Header(buf.asIntBuffer)

        // read index
        raf.seek(header.indexOffsetLocation.toLong)
        managed(raf.getChannel()) acquireAndGet { fc =>
          val builder = new scala.collection.immutable.VectorBuilder[StreamedEntry]()
          builder.sizeHint(header.indexEntryCount.toInt)

          val indexBuf = allocLEBB(IndexBufferSize min header.indexSize.toInt)
          var pos, i = UInt(0)
          while (pos < header.indexSize) {
            val count = fc.read(indexBuf)
            if (count == -1) throw new EOFException("DBPF entry index reaches beyond end of file")
            indexBuf.flip()
            while (indexBuf.remaining() >= 20 && i < header.indexEntryCount) {
              val tgi = Tgi(indexBuf.getInt(), indexBuf.getInt(), indexBuf.getInt())
              val offset = UInt(indexBuf.getInt())
              val size = UInt(indexBuf.getInt())
              val entry = new StreamedEntry(file, tgi, offset, size, header.dateModified)
              builder += entry
              i += UInt(1)
            }
            indexBuf.compact();
            pos += UInt(count)
          }

          new DbpfFile(file, header, builder.result())
        }
      }
    }
  }

  /** Writes a `DbpfFile` back to a file.
    *
    * If the target `file` already exists, the `DbpfFile` will be written to an
    * intermediate temporary file, which upon success will be renamed to `file`.
    * This ensures that `StreamedEntries` can be written to the same file they
    * are read from. After completion, those `StreamedEntries` are out-of-date
    * and should be replaced by the `DbpfFile` that is returned, which contains
    * the current entries of that file.
    *
    * @param entries the entries to be written
    * @param file the target file
    * @param dateCreated the creation date of the DbpfFile. This is a value of
    * the DBPF file header which should be preserved if possible. If no value is
    * given, this defaults to the current time.
    * @return a new `DbpfFile` that represents the current state of the `file`
    *
    * @throws DbpfStreamStreamOutOfDateException if the source file has been
    * modified since a StreamedEntry was created
    * @throws FileNotFoundException if the target file is inaccessible
    * @throws IOException in case of other IO errors
    */
  def write(
      entries: TraversableOnce[DbpfEntry],
      file: JFile,
      dateCreated: UInt = UInt((System.currentTimeMillis() / 1000).toInt))
        (implicit eh: ExceptionHandler): eh.![DbpfFile, IOException] = eh wrap {

    val (header, indexList) = if (!file.exists) {
      writeImpl(entries, file, dateCreated)
    } else {
      // writing to a temporary buffer ensures that entries can stream from the destination file
      val tmpFile = java.io.File.createTempFile(file.getName + "_", ".tmp", file.getParentFile)
      try {
        val result = writeImpl(entries, tmpFile, dateCreated)
        // attempts to move source to target (twice); especially on Windows,
        // rename is problematic, if the target already exists.
        // Mind that in rare cases this implementation may lead to loss of both
        // files, i.e. if target gets deleted, subsequent rename fails, and temp
        // file gets deleted, subsequently.
        def move(source: JFile, target: JFile): Boolean = {
          if (source.renameTo(target)) {
            true
          } else if (target.exists) {
            if (target.delete) {
              source.renameTo(target) || (throw new DbpfIoException("Failed to rename temp file to destination file: " + target))
            } else {
              throw new DbpfIoException("Failed to delete pre-existing destination file: " + target)
            }
          } else {
            throw new DbpfIoException("Failed to rename temp file to destination file: " + target)
          }
        }
        move(tmpFile, file)
        result
      } finally {
        tmpFile.delete()
      }
    }

    val streamedEntries: IndexedSeq[StreamedEntry] = indexList.map { ie =>
      new StreamedEntry(file, ie.tgi, ie.offset, ie.size, header.dateModified)
    }(scala.collection.breakOut)
    new DbpfFile(file, header, streamedEntries)
  }

  private def writeImpl(entries: TraversableOnce[DbpfEntry], file: JFile, dateCreated: UInt): (Header, Iterable[IndexEntry]) = {

    def buildDir(dirData: Iterable[IndexEntry]): Input[Byte] = {
      val buf = allocLEBB(dirData.size * 16)
      for (e <- dirData; decompSize <- e.decompressedSize) {
        buf.putInt(e.tgi.tid)
        buf.putInt(e.tgi.gid)
        buf.putInt(e.tgi.iid)
        buf.putInt(decompSize.toInt)
      }
      assert(buf.remaining() == 0)
      new ByteArrayInput(buf.array())
    }
    def buildIndex(indexData: Iterable[IndexEntry]): Input[Byte] = {
      val buf = allocLEBB(indexData.size * 20)
      for (e <- indexData) {
        buf.putInt(e.tgi.tid)
        buf.putInt(e.tgi.gid)
        buf.putInt(e.tgi.iid)
        buf.putInt(e.offset.toInt)
        buf.putInt(e.size.toInt)
      }
      assert(buf.remaining() == 0)
      new ByteArrayInput(buf.array())
    }

    import scala.collection.mutable.ArrayBuffer
    val writeList = entries match {
      case indexed: scala.collection.IndexedSeq[_] => new ArrayBuffer[WrappedDbpfInput](indexed.size + 1)
      case _ => new ArrayBuffer[WrappedDbpfInput]()
    }

    managed(new RandomAccessFile(file, "rw")) acquireAndGet { raf =>
      raf.seek(HeaderSize)
      raf.setLength(HeaderSize.toLong) // trim to minimum size
      managed(new ByteOutput(new FileOutputStream(raf.getFD()))) acquireAndGet { output =>
        // pump all entries to file, ignore dirs
        managed {
          new SequenceInput(new scala.collection.AbstractIterator[WrappedDbpfInput] {
            private[this] val iter: Iterator[DbpfEntry] = entries.withFilter(_.tgi != Tgi.Directory)
            def hasNext: Boolean = iter.hasNext
            def next: WrappedDbpfInput = {
              val n = new WrappedDbpfInput(iter.next)
              writeList += n // lazily collect results in ArrayBuffer
              n
            }
          })
        } acquireFor { input =>
          input > output
        }

        // update offsets and index entry data
        var offset = writeList.foldLeft(UInt(HeaderSize)) { (offset, wrapper) =>
          wrapper.offset = Some(offset)
          offset + wrapper.indexEntry.size
        }

        // directory file
        val dirList = writeList.view.map(_.indexEntry).filter(_.decompressedSize.isDefined).force
        val dirIndexEntry: Option[IndexEntry] = if (dirList.isEmpty) None else {
         // write dir to file
          managed(buildDir(dirList)) acquireAndGet (_ > output)
          Some(new IndexEntry(Tgi.Directory, offset, UInt(dirList.size) * UInt(16), None))
        }
        dirIndexEntry foreach { offset += _.size }

        // dbpf index
        val indexList = writeList.map(_.indexEntry) ++= dirIndexEntry
        managed(buildIndex(indexList)) acquireAndGet (_ > output)

        val header = new Header(
          dateCreated = dateCreated,
          dateModified = UInt((System.currentTimeMillis() / 1000).toInt),
          indexEntryCount = UInt(indexList.size),
          indexOffsetLocation = offset,
          indexSize = UInt(indexList.size) * UInt(20))
        // write header
        raf.seek(0)
        val headerArray = header.toArray
        raf.write(headerArray, 0, headerArray.length)

        (header, indexList)
      }
    }
  }

  private class IndexEntry(val tgi: Tgi, val offset: UInt, val size: UInt, val decompressedSize: Option[UInt])

  /** This class wraps the input of a DbpfEntry and thereby collects information
    * like size and decompressed size, which can be retrieved from the indexEntry
    * after the input has been fully consumed.
    */
  private class WrappedDbpfInput(private[this] var entry: DbpfEntry) extends AbstractByteInput {
    private val tgi = entry.tgi
    private var input: Input[Byte] = null
    private val headerBuf = ByteBuffer.allocate(9)
    private var pos = 0
    private var endReached = false
    var offset: Option[UInt] = None

    def initialize(): this.type = { input = entry.input(); entry = null; this } // get rid of reference for GC
    def close(): Unit = if (input != null) {
      input.close()
      input = null // get rid of reference for GC
    }
    def ready(): Boolean = input.ready()

    override def readBlock(array: Array[Byte], offset: Int, length: Int): Int = {
      val count = input.readBlock(array, offset, length)
      if (count >= 0) pos += count
      else endReached = true

      if (headerBuf.remaining() > 0 && count > 0) {
//        println("started reading " + tgi)
        headerBuf.put(array, offset, headerBuf.remaining() min count)
      }
      count
    }

    lazy val indexEntry = {
      assert(offset.isDefined && endReached, s"offset $offset endReached $endReached tgi $tgi")
      val decompSize =
        if (headerBuf.remaining() == 0) {
          DbpfPackager.decompressedSize(headerBuf.array())
        } else {
          None // entry is too short for being compressed
        }
      new IndexEntry(tgi, offset.get, UInt(pos), decompSize)
    }
  }

  private[scdbpf] abstract class AbstractByteInput extends Input[Byte] {
    def read(): Option[Byte] = {
      val buf = new Array[Byte](1)
      readBlock(buf, 0, 1) match {
        case -1 => None
        case count => assert(count == 1); Some(buf(0))
      }
    }
  }

  private class SequenceInput(iter: Iterator[WrappedDbpfInput], fully: Boolean = true) extends AbstractByteInput {
    var current = if (iter.hasNext) iter.next().initialize() else null

    def close(): Unit = {
      if (current != null) {
        current.close()
        iter.foreach(_.close())
      }
    }

    def ready(): Boolean = if (current == null) true else current.ready()

    override def readBlock(array: Array[Byte], offset: Int, length: Int): Int = {
      val len = if (length == -1) array.length - offset else length
      if (offset < 0 || len < 0 || offset + len > array.length) {
        throw new IndexOutOfBoundsException(f"offset: $offset, length: $len, array.length: ${array.length}")
      }
      var read = 0
      var pos = offset
      var contin = true
      while (current != null && read < len && contin) {
        contin = true
        val count = current.readBlock(array, pos, len - read)
        if (count == -1) {
          current.close()
          current = if (iter.hasNext) iter.next().initialize() else null
        } else {
          read += count
          pos += count
          if (!fully) contin = false
        }
      }
      // result
      if (current == null && read == 0) {
        -1
      } else {
        read
      }
    }
  }

}
