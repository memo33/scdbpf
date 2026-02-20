package io.github.memo33
package scdbpf

import DbpfUtil._

/** The base trait for the content type of `DbpfEntries`.
  *
  * @see [[BufferedEntry]]
  *
  * @define EXCEPTIONHANDLER
  * An `ExceptionHandler` needs to be brought into scope via imports (either
  * `strategy.throwExceptions` or `strategy.captureExceptions` from the
  * `scdbpf` package).
  */
trait DbpfType {
  /** the uncompressed raw byte data of the entry */
  def unsafeArray: Array[Byte]

  /** Deprecated: Prefer `convertTo` instead.
    *
    * Converts this type to `B`. The implicit converter is usually provided by
    * the companion object of `B`.
    *
    * $EXCEPTIONHANDLER
    *
    * @tparam B the type which this type gets converted to
    * @throws DbpfDecodeFailedException if this type cannot be converted to
    * type `B` structurally
    */
  def convert[B <: DbpfType](implicit eh: ExceptionHandler, conv: Converter[DbpfType, B]):
    eh.![B, DbpfDecodeFailedException] = eh wrap conv(this)

  /** Convert this type to `B`.
    *
    * $EXCEPTIONHANDLER
    *
    * @tparam B the type which this type gets converted to
    * @param dbpfType (the companion object of) the type which this type gets converted to
    * @throws DbpfDecodeFailedException if this type cannot be converted to
    * type `B` structurally
    */
  def convertTo[B <: DbpfType](dbpfType: DbpfTypeCompanion[B])(implicit eh: ExceptionHandler): eh.![B, DbpfDecodeFailedException] = {
    eh wrap dbpfType.converter(this)
  }
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
class RawType protected (val unsafeArray: Array[Byte]) extends DbpfType {
  assert(!DbpfPackager.isCompressed(unsafeArray))
}

object RawType {
  /** Creates a [[RawType]] from an array; decompresses the data, if it is
    * compressed.
    */
  def apply(data: Array[Byte]): RawType = new RawType(DbpfPackager.decompress(data))
}

/** This trait defines the converters that should be implemented by companion
  * objects of `DbpfType`s.
  */
trait DbpfTypeCompanion[B <: DbpfType] extends WithContentConverter[B] {
  def converter: Converter[DbpfType, B]
  def contentConverter: Converter[BufferedEntry[DbpfType], BufferedEntry[B]] = genericConverter(converter)
}
