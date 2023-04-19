/* Forked from Rapture I/O Library Version 0.9.1 and
 * Rapture Core Library Version 0.9.0 to facilitate upgrading Scala version.
 * (released under the Apache License, Version 2.0)
 */
package io.github.memo33
package scdbpf.compat

import scala.reflect.ClassTag
import java.io.{InputStream, BufferedInputStream, ByteArrayInputStream, OutputStream, BufferedOutputStream}
import scala.language.higherKinds

/** This trait exisist for compatibility between scala 2.12 and 2.13
  * collections, since 2.13 collections define a map method that requires an
  * explicit override.
  */
private[compat] trait MapOverride[@specialized(Byte, Char) Data, C[_]] {
  def map[T](fn: Data => T): C[T]
}

/** An Input provides an incoming stream of data */
trait Input[@specialized(Byte, Char) Data] extends Seq[Data] with MapOverride[Data, Input] { thisInput =>

  private var beingHandled = false

  override def toString() = "<input>"

  def length: Int = throw new Exception("Cannot calculate length of a stream")

  def apply(n: Int) = {
    for(i <- 0 until n) read()
    read().get
  }

  // def typed(mime: MimeTypes.MimeType) = new Input[Data] with TypedInput {
  //   def mimeType = mime
  //   def ready() = thisInput.ready()
  //   def close() = thisInput.close()
  //   def read() = thisInput.read()
  // }

  def iterator: Iterator[Data] = new Iterator[Data] {
    private var nextVal: Option[Data] = read()
    def hasNext = nextVal.isDefined
    def next() = {
      val x = nextVal.get
      nextVal = read()
      x
    }
  }

  /** Returns whether the stream can be read without blocking */
  def ready(): Boolean

  /** Reads a single item of data from the input stream.  Note that each call to this method
    * will result in a new instance of `Some` to be constructed, so for reading larger block, use
    * the `readBlock` method which may be implemented more efficiently. */
  def read(): Option[Data]

  /** Default implementation for reading a block of data from the input stream into the specified
    * array.
    *
    * The basic implementation is provided for convenience, though it is not an efficient
    * implementation and results in large numbers of boxing objects being created unnecessarily.
    * Subclasses of Input should provide their own implementations of writeBlock.
    *
    * @param array The array into which content should be read
    * @param offset The offset to the position within the array that data should start to be
    *        written.  Defaults to the start of the array.
    * @param length The length of data to read from the stream into the array.
    * @return The number of items of data transferred */
  def readBlock(array: Array[Data], offset: Int = 0, length: Int = -1): Int = {

    val end = if(length < 0) (array.length - offset) else (offset + length)

    read() match {
      case None => -1
      case Some(c) =>
        array(offset) = c
        var i = offset + 1
        var continue = true
        while(i < end && continue) {
          read() match {
            case None =>
              continue = false
            case Some(c) =>
              array(i) = c
              i += 1
          }
        }

        i - offset
    }
  }

  /** Closes the input stream so that no further data will be provided. */
  def close(): Unit

  /** Pumps data from this `Input` to the specified `Output` until the end of the stream is
    * reached.
    *
    * @param out The output stream to receive data pumped from this `Input` */
  def pumpTo(out: Output[Data])(implicit mf: ClassTag[Data]): Int = {
    val buf = new Array[Data](65536)
    var len = readBlock(buf)
    var count = 0
    while(len >= 0) {
      out.writeBlock(buf, length = len)
      count += len
      len = readBlock(buf)
    }
    count
  }

  def > (out: Output[Data])(implicit mf: ClassTag[Data]): Int = pumpTo(out)

  override def map[T](fn: Data => T): Input[T] = new Input[T] {
    def read(): Option[T] = thisInput.read().map(fn)
    def ready(): Boolean = thisInput.ready()
    def close(): Unit = thisInput.close()
  }

  /** Maps elements of the input stream to zero, one or many elements, producing a new input
    * stream. */
  def flatMap[T](fn: Data => Seq[T]): Input[T] = new Input[T] {
    private var buf: Seq[T] = Nil
    private var cur = 0
    private var avail = 0

    def read(): Option[T] = if(cur == avail) {
      cur = 0
      avail = 0
      thisInput.read().map(fn) match {
        case None => None
        case Some(xs) =>
          if(xs.isEmpty) read()
          else if(xs.length == 1) xs.headOption
          else {
            avail = xs.length
            cur += 1
            buf = xs
            xs.headOption
          }
      }
    } else {
      cur += 1
      Some(buf(cur - 1))
    }

    def ready(): Boolean = cur < avail || thisInput.ready()

    def close(): Unit = {
      cur = 0
      avail = 0
      thisInput.close()
    }
  }

  override def foreach[U](fn: Data => U): Unit = {
    var next: Option[Data] = read()
    while(next != None) {
      fn(next.get)
      next = read()
    }
  }
}

object Input {
  def slurpBytes(input: Input[Byte])(implicit eh: ExceptionHandler): eh.![Array[Byte], java.io.IOException] = eh.wrap {
    scdbpf.DbpfUtil.slurpBytes(input)
  }
}

/** Defines a generic output stream */
trait Output[@specialized(Byte, Char) Data] {
  private var beingHandled = false

  /** Writes one item of data to this stream
    *
    * @param data The data to be written */
  def write(data: Data): Unit

  /** Writes a block of data from an array to the stream
    *
    * The basic implementation is provided for convenience, though it is not an efficient
    * implementation and results in large numbers of boxing objects being created unnecessarily.
    * Subclasses of Output should provide their own implementations of writeBlock.
    *
    * @param array the Array containing the data to be written to the stream
    * @param offset The offset to the position within the array that data should start to be
    *        read from when writing to the stream.  Defaults to the start of the array.
    * @param length The length of data to write from the array into the stream. Defaults to the
    *        remainder of the array.
    * @return The number of data items written. */
  def writeBlock(array: Array[Data], offset: Int = 0, length: Int = -1): Int = {

    val end = if(length < 0) (array.length - offset) else (offset + length)
    array.slice(offset, end).foreach(write)

    end - offset
  }

  /** Flushes the stream */
  def flush(): Unit

  /** Closes the stream */
  def close(): Unit
}


/** Wraps a `java.io.InputStream` as an `Input[Byte]` */
class ByteInput(in: InputStream) extends Input[Byte] {

  private val bin = new BufferedInputStream(in)

  // FIXME: This might be really slow
  def ready() = bin.available() > 0

  def read() = bin.read() match {
    case -1 => None
    case x => Some(x.toByte)
  }

  override def readBlock(array: Array[Byte], offset: Int = 0, length: Int = -1): Int =
    bin.read(array, offset, if(length == -1) (array.length - offset) else length)

  def close() = in.close()

  override def toString() = "<byte input>"
}

/** Makes an `Array[Byte]` viewable as an `Input[Byte]` */
case class ByteArrayInput(array: Array[Byte]) extends ByteInput(new ByteArrayInputStream(array))

/** Wraps a `java.io.OutputStream` into an `Output[Byte]`
  *
  * @param out The `java.io.OutputStream` to be wrapped */
class ByteOutput(out: OutputStream) extends Output[Byte] {

  private val bout = new BufferedOutputStream(out)

  def write(b: Byte) = bout.write(b)

  def flush(): Unit = bout.flush()
  def close(): Unit = bout.close()

  override def toString() = "<byte output>"

  override def writeBlock(array: Array[Byte], offset: Int = 0, length: Int = -1): Int = {
    val len = if(length == -1) (array.length - offset) else length
    bout.write(array, offset, len)
    bout.flush()
    len
  }
}

import scala.language.higherKinds
import scala.annotation.implicitNotFound
import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}

@implicitNotFound(msg = "No exception handler was available. Please import "+
  "a member of scdbpf.strategy, e.g. strategy.throwExceptions.")
trait ExceptionHandler { eh =>
  type ![+_, _ <: Exception]
  def wrap[T, E <: Exception: ClassTag](t: => T): ![T, E]

  def compose(eh2: ExceptionHandler) = new ExceptionHandler {
    type ![+T, E <: Exception] = eh.![eh2.![T, E], E]
    def wrap[T, E <: Exception: ClassTag](t: => T): ![T, E] =
      eh.wrap(eh2.wrap(t))
  }
}

object raw extends strategy.ThrowExceptions

object strategy {

  implicit def throwExceptions = new ThrowExceptions
  class ThrowExceptions extends ExceptionHandler {
    type ![+T, E <: Exception] = T
    def wrap[T, E <: Exception: ClassTag](t: => T): T = t
  }

  implicit def explicit = new ExplicitReturns
  class ExplicitReturns extends ExceptionHandler {
    type ![+T, E <: Exception] = Explicit[T, E]
    def wrap[T, E <: Exception: ClassTag](t: => T): Explicit[T, E] =
      new Explicit[T, E](t)
  }

  class Explicit[+T, E <: Exception: ClassTag](t: => T) {
    def get: T = t
    def opt: Option[T] = discardExceptions.wrap(t)
    def getOrElse[T2 >: T](t: T2): T2 = opt.getOrElse(t)
    //def default[T](implicit default: Default[T]) = useDefaults.wrap(t).apply()
    def either: Either[E, T] = captureExceptions.wrap(t)
    def attempt: Try[T] = returnTry.wrap(t)
    // def time[D: TimeSystem.ByDuration] = timeExecution.wrap(t)
    def future(implicit ec: ExecutionContext): Future[T] = returnFutures.wrap(t)

    override def toString = "[unexpanded result]"
  }

  implicit def captureExceptions = new CaptureExceptions
  class CaptureExceptions extends ExceptionHandler {
    type ![+T, E <: Exception] = Either[E, T]
    def wrap[T, E <: Exception: ClassTag](t: => T): Either[E, T] =
      try Right(t) catch {
        case e: E => Left(e)
        case e: Throwable => throw e
      }

    override def toString = "[strategy.captureExceptions]"
  }

  implicit def returnTry = new ReturnTry
  class ReturnTry extends ExceptionHandler {
    type ![+T, E <: Exception] = Try[T]
    def wrap[T, E <: Exception: ClassTag](t: => T): Try[T] = Try(t)

    override def toString = "[strategy.returnTry]"
  }

  implicit val kcaco = new Kcaco
  class Kcaco extends ExceptionHandler {
    type ![+T, E <: Exception] = T
    def wrap[T, E <: Exception: ClassTag](t: => T): T =
      try t catch { case e: Exception => null.asInstanceOf[T] }

    override def toString = "[strategy.kcaco]"
  }

  implicit val discardExceptions = new DiscardExceptions
  class DiscardExceptions extends ExceptionHandler {
    type ![+T, E <: Exception] = Option[T]
    def wrap[T, E <: Exception: ClassTag](t: => T): Option[T] =
      try Some(t) catch { case e: Exception => None }

    override def toString = "[strategy.discardExceptions]"
  }

  implicit def returnFutures(implicit ec: ExecutionContext) = new ReturnFutures
  class ReturnFutures(implicit ec: ExecutionContext) extends ExceptionHandler {
    type ![+T, E <: Exception] = Future[T]
    def wrap[T, E <: Exception: ClassTag](t: => T): Future[T] = Future { t }

    override def toString = "[strategy.returnFutures]"
  }

  // implicit def timeExecution[D: TimeSystem.ByDuration] = new TimeExecution[D]
  // class TimeExecution[D: TimeSystem.ByDuration] extends ExceptionHandler {
  //   val ts = ?[TimeSystem.ByDuration[D]]
  //   type ![+T, E <: Exception] = (T, D)
  //   def wrap[T, E <: Exception: ClassTag](r: => T): (T, D) = {
  //     val t0 = System.currentTimeMillis
  //     (r, ts.duration(t0, System.currentTimeMillis))
  //   }

  //   override def toString = "[strategy.timeExecution]"
  // }

  /*class Defaulting[-T](t: => T) {
    def apply[T]()(implicit default: Default[T]) =
      try t catch { case e: Exception => ?[Default[T]].default }
  }

  implicit def useDefaults = new UseDefaults
  class UseDefaults extends ExceptionHandler {
    type ![+T, E <: Exception] = Defaulting[T]
    def wrap[T, E <: Exception: ClassTag](t: => T): Defaulting[T] = new Defaulting(t)
    override def toString = "[strategy.useDefaults]"
  }*/
}
