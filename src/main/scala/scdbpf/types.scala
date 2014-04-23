package scdbpf

import rapture.io._
import resource._
import DbpfUtil._

/** The base trait for the content type of `DbpfEntries`.
  *
  * @see [[BufferedEntry]]
  *
  * @define EXCEPTIONHANDLER
  * An `ExceptionHandler` needs to be brought into scope via imports (either
  * `strategy.throwExceptions` or `strategy.captureExceptions` from the
  * `rapture.io` package).
  */
trait DbpfType {
  /** the uncompressed raw byte data of the entry */
  protected def data: Array[Byte]
  private[scdbpf] final def dataView: Array[Byte] = data // to make the data visible within this package

  /** Converts this type to `B`. The implicit converter is usually provided by
    * the companion object of `B`.
    *
    * $EXCEPTIONHANDLER
    *
    * @tparam B the type which this type gets converted to
    * @throws DbpfDecodeFailedException if this type cannot be converted to
    * type `B` structurally
    */
  def convert[B <: DbpfType](implicit eh: ExceptionHandler, conv: Converter[DbpfType, B]):
    eh.![DbpfDecodeFailedException, B] = eh except conv(this)
}

/** A raw type that does not represent any specific format. Its data is backed
  * by an array. As such, it is also suited as a super class of specialized
  * types which are backed by an array.
  *
  * Instances of this class my be obtained via the companion object.
  *
  * @note This is one of the few spots where the raw backing array gets exposed.
  * If instances of this class or created from instances of this class are meant
  * to be immutable, it is required to ensure that this array does not get
  * modified.
  */
class RawType protected (protected val data: Array[Byte]) extends DbpfType {
  assert(!DbpfPackager.isCompressed(data))
}

object RawType {
  /** Creates a [[RawType]] from an array; decompresses the data, if it is
    * compressed.
    */
  def apply(data: Array[Byte]): RawType = new RawType(DbpfPackager.decompress(data))
}
