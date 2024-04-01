package io.github.memo33
package scdbpf

import DbpfUtil._
import DbpfProperty.{PropList, Property}
import scala.collection.immutable.SortedMap
import scala.collection.compat._
import io.github.memo33.passera.unsigned._


sealed trait Exemplar extends DbpfType {
  /** the parent cohort tgi */
  val parent: Tgi
  val properties: SortedMap[UInt, PropList]
  /** determines whether this type is an exemplar or a cohort */
  val isCohort: Boolean

  /** The ordering of properties needs to be [[Exemplar.ordering]], or else a new map will be built. */
  def copy(parent: Tgi = parent, isCohort: Boolean = isCohort, properties: SortedMap[UInt, PropList] = properties): Exemplar = {
    if (properties.ordering == Exemplar.ordering) {
      new Exemplar.FreeExemplar(parent, isCohort, properties)
    } else {
      Exemplar(parent, isCohort, properties.iterator)
    }
  }

  /* for convenience, wraps the prop values with the prop ID */
  def apply(id: UInt): Property = (id, properties(id))
}

object Exemplar extends WithContentConverter[Exemplar] {

  def apply(parent: Tgi = Tgi.Blank, isCohort: Boolean, props: IterableOnce[Property]): Exemplar = {
    var builder = SortedMap.newBuilder[UInt, PropList](ordering)
    props.iterator.foreach(tup => builder += tup)
    new FreeExemplar(parent, isCohort, builder.result())
  }

  implicit def converter: Converter[BufferedEntry[DbpfType], BufferedEntry[Exemplar]] = new Converter[BufferedEntry[DbpfType], BufferedEntry[Exemplar]] {
    def apply(from: BufferedEntry[DbpfType]): BufferedEntry[Exemplar] = {
      try {
        from.copy(content = new BufferedExemplar(from.content.dataView, from.tgi matches Tgi.Cohort))
      } catch {
        case e @ (_: NoSuchElementException
                 |_: java.nio.BufferUnderflowException
                 |_: IllegalArgumentException
                 |_: IndexOutOfBoundsException
                 |_: NumberFormatException
                 |_: org.parboiled.errors.ParserRuntimeException) =>
          throw new DbpfDecodeFailedException(e.toString, e)
      }
    }
  }
  def contentConverter = converter

  /** unsigned int ordering; to be used for properties map */
  val ordering = UIntOrdering

  private lazy val parser = new PropertyParser() // needs to be locked for concurrent access

  private class BufferedExemplar(arr: Array[Byte], val isCohort: Boolean) extends RawType(arr) with Exemplar {
    val (parent, properties) = {
      import MagicNumber._
      val binType = if (isCohort) CQZB else EQZB
      val textType = if (isCohort) CQZT else EQZT

      val buf = wrapLEBB(data)
      val sig = buf.getInt()
      val version = buf.getInt()

      if (sig == binType && version == `1###`) {
        val cohort = Tgi(buf.getInt(), buf.getInt(), buf.getInt())
        val count = buf.getInt()
        var props = SortedMap.empty[UInt, PropList](ordering)
        for (_ <- 1 to count) {
          val id = UInt(buf.getInt())
          val p = DbpfProperty.decode(buf, id)
          props = props.updated(id, p)
        }
        if (buf.remaining() != 0) {
          throw new DbpfDecodeFailedException("Buffer content not consumed entirely")
        }
        (cohort, props)
      } else if (sig == textType && version == `1###`) {
        val text = new String(data, asciiEncoding)
        parser.synchronized {
          val ex = parser.parseExemplar(text)
          (ex.parent, ex.properties)
        }
      } else {
        val msg =
          if (sig == binType || sig == textType) f"Unknown exemplar version 0x$version%08X"
          else f"Unknown filetype 0x$sig%08X"
        throw new DbpfDecodeFailedException(msg)
      }
    }
  }

  private class FreeExemplar(val parent: Tgi, val isCohort: Boolean, val properties: SortedMap[UInt, PropList]) extends Exemplar {
    protected lazy val data = {
      val binaryLength = properties.valuesIterator.foldLeft(24)(_ + _.binaryLength)
      val buf = allocLEBB(binaryLength)
      buf.putInt(if (isCohort) MagicNumber.CQZB else MagicNumber.EQZB)
      buf.putInt(MagicNumber.`1###`)
      buf.putInt(parent.tid)
      buf.putInt(parent.gid)
      buf.putInt(parent.iid)
      buf.putInt(properties.size)
      for ((id, p) <- properties.iterator) {
        p.encode(buf, id)
      }
      assert(buf.remaining() == 0)
      buf.array
    }
  }
}
