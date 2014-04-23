package scdbpf

import passera.unsigned.UInt
import java.nio.ByteBuffer
import DbpfUtil.MagicNumber

/** Provides method for QFS compressing and decompressing.
  */
object DbpfPackager {

  private def isCompressed(leBuf: ByteBuffer): Boolean = {
    if (leBuf.capacity < 9) {
      false
    } else {
      val sig = leBuf.getShort(4)
      if (sig == MagicNumber.QFS) {
        val fType = leBuf.getInt(0)
        // there is an s3d file in sc1.dat with sig == QFS,
        // but which in fact is not compressed, so this is
        // a non-failproof workaround
        fType != MagicNumber.`3DMD`
      } else {
        false
      }
    }
  }

  private[scdbpf] def isCompressed(header: Array[Byte]): Boolean =
    isCompressed(DbpfUtil.wrapLEBB(header))

  private[scdbpf] def decompressedSize(header: Array[Byte]): Option[UInt] = {
    val buf = DbpfUtil.wrapLEBB(header)
    if (!isCompressed(buf)) {
      None
    } else {
      // decompressed size is stored big endian
      val decompSize =
        (buf.get(6) & 0xff) * 0x10000 +
        (buf.get(7) & 0xff) * 0x100 +
        (buf.get(8) & 0xff)
      Some(UInt(decompSize))
    }
  }

  /** Compresses the data. If `dData` is already compressed or the compressed
    * data would be larger than the uncompressed data, `dData` is returned.
    * Otherwise, a new array containing the compressed data will be created and
    * returned.
    */
  def compress(dData: Array[Byte]): Array[Byte] = {
    if (dData.length > 9 && dData.length <= 0xFFFFFF && !isCompressed(dData)) {
      val cData = QfsCompression.compress(dData)
      if (cData.length < dData.length) cData else dData
    } else {
      dData
    }
  }

  /** Decompresses the data. If `cData` is not QFS compressed, `cData` is
    * returned. Otherwise, a new array containing the decompressed data will be
    * created and returned.
    *
    * @throws DbpfDecodeFailedException if QFS compressed data is malformed
    */
  def decompress(cData: Array[Byte]): Array[Byte] = {
    try {
      val decompSize = decompressedSize(cData)
      if (decompSize.isEmpty) {
        cData
      } else {
        val dData = new Array[Byte](decompSize.get.toInt)
        QfsDecompression.decompress(cData, dData)
        dData
      }
    } catch {
      case e: IndexOutOfBoundsException => throw new DbpfDecodeFailedException("malformed compressed data", e)
    }
  }
}
