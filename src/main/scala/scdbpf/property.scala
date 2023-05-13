package io.github.memo33
package scdbpf

import java.nio.ByteBuffer
import java.nio.ByteOrder.{LITTLE_ENDIAN => LittleEndian}
import io.github.memo33.passera.unsigned._
import scala.reflect.ClassTag
import scala.collection.mutable.{Builder, ArrayBuilder}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import DbpfProperty.ValueType
import DbpfUtil.asciiEncoding


object DbpfProperty {

  /** A `PropList` can contain a single or multiple values. To access them,
    * match against `Single` and `Multi`:
    * {{{
    * import DbpfProperty._
    * (p: PropList) match {
    *   case Single(value) => "value is the single value of this property"
    *   case Multi(values) => "values is an IndexedSeq"
    * }
    * }}}
    * You can also match on particular value types:
    * {{{
    * import DbpfProperty._, ValueType._
    * (p: PropList) match {
    *   case String(Single(value)) => "value is a string"
    *   case Float32(Single(value)) => "value is a float"
    *   case Sint32(Multi(values)) => "values is an IndexedSeq[Int]"
    *   case Uint32(Multi(values)) => "values is an IndexedSeq[UInt]"
    *   case _ => "otherwise"
    * }
    * }}}
    */
  sealed trait PropertyList[A] {
    val valueType: ValueType[A]
    def apply(idx: Int): A
    private[scdbpf] def binaryLength: Int
    private[scdbpf] def encode(buf: ByteBuffer, id: UInt): Unit
  }

  type PropList = PropertyList[_]
  type Property = (UInt, PropList)
  type ValueType[D] = ValueType.ValueType[D]
  type NumericalValueType[D] = ValueType.ValueType[D] with ValueType.Numerical[D]

  object ValueType extends Enumeration {

    sealed trait Numerical[A] { this: ValueType[A] =>
      import scala.language.higherKinds
      def unapply[C[_] <: PropertyList[_]](prop: C[_]): Option[C[A] with Numerical[A]] = {
        if (prop.valueType != this) None else {
          Some(prop.asInstanceOf[C[A] with Numerical[A]])
        }
      }

      private[DbpfProperty] def buildProperty(buf: ByteBuffer, count: Option[Int]): PropertyList[A] = count match {
        case Some(cnt) =>
          val values: IndexedSeq[A] = IndexedSeq.fill(cnt)(getValue(buf))
          new Multi(values)(this)
        case None =>
          new Single(getValue(buf))(this)
      }
    }

    sealed private[DbpfProperty] trait NonNumerical { this: ValueType[String] =>
      def unapply(prop: PropertyList[_]): Option[Single[String]] = {
        if (prop.valueType != this) None else {
          Some(prop.asInstanceOf[Single[String]])
        }
      }

      private[DbpfProperty] def buildProperty(buf: ByteBuffer, count: Option[Int]): Single[String] = {
        assert(count.isDefined)
        val bytes = new Array[Byte](count.get)
        buf.get(bytes)
        new Single(new String(bytes, asciiEncoding))(ValueType.String)
      }
    }

    sealed abstract class ValueType[A] private[DbpfProperty](
      private[DbpfProperty] val valueId: Short,
      private[DbpfProperty] val wordLength: Int,
      protected val getValue: ByteBuffer => A,
      private[DbpfProperty] val putValue: (ByteBuffer, A) => Unit)
        (implicit private[DbpfProperty] val tag: ClassTag[A]) extends super.Val {

      private[DbpfProperty] def buildProperty(buf: ByteBuffer, count: Option[Int]): PropertyList[A]
    }

    implicit val Uint8   = new ValueType[UByte]  (0x0100, 1, buf => UByte(buf.get())      , (buf, a) => buf.put(a.toByte)              ) with Numerical[UByte]
    implicit val Uint16  = new ValueType[UShort] (0x0200, 2, buf => UShort(buf.getShort()), (buf, a) => buf.putShort(a.toShort)        ) with Numerical[UShort]
    implicit val Uint32  = new ValueType[UInt]   (0x0300, 4, buf => UInt(buf.getInt())    , (buf, a) => buf.putInt(a.toInt)            ) with Numerical[UInt]
    implicit val Sint32  = new ValueType[Int]    (0x0700, 4, buf => buf.getInt()          , (buf, a) => buf.putInt(a)                  ) with Numerical[Int]
    implicit val Sint64  = new ValueType[Long]   (0x0800, 8, buf => buf.getLong()         , (buf, a) => buf.putLong(a)                 ) with Numerical[Long]
    implicit val Float32 = new ValueType[Float]  (0x0900, 4, buf => buf.getFloat()        , (buf, a) => buf.putFloat(a)                ) with Numerical[Float]
    implicit val Bool    = new ValueType[Boolean](0x0B00, 1, buf => (buf.get() & 0x1) == 1, (buf, a) => buf.put((if(a) 1 else 0): Byte)) with Numerical[Boolean]
    implicit val String  = new ValueType[String] (0x0C00, 0, null                         , null                                       ) with NonNumerical

    def withId(id: Short): Option[ValueType[_]] = id match {
      case 0x0100 => Some(Uint8)
      case 0x0200 => Some(Uint16)
      case 0x0300 => Some(Uint32)
      case 0x0700 => Some(Sint32)
      case 0x0800 => Some(Sint64)
      case 0x0900 => Some(Float32)
      case 0x0B00 => Some(Bool)
      case 0x0C00 => Some(String)
      case _ => None
    }
  }

  private[scdbpf] def decode(buf: ByteBuffer, id: UInt): PropList = {
    assert(buf.order() == LittleEndian)
    val valTypeId = buf.getShort()
    val valType = ValueType.withId(valTypeId)
    val keyType = buf.getShort()
    val count =
      if (keyType == 0x80) {
        buf.get() // unused byte
        Some(buf.getInt())
      } else if (keyType == 0) {
        val rep = buf.get() // unused?
        assert(rep == 0 && valType.get != ValueType.String, f"prop id: ${id.toInt}%08X,val type: $valType, rep: $rep")
        None
      } else {
        throw new DbpfDecodeFailedException(f"Property key type was $keyType%04X")
      }

    valType match {
      case Some(vt) => vt.buildProperty(buf, count)
      case None => throw new DbpfDecodeFailedException(f"Unknown value type ID 0x$valTypeId%04X for property ID 0x${id.toInt}%08X")
    }
  }

  /** (for convenience) calls Multi.apply() */
  def apply[A : NumericalValueType](values: Seq[A]): Multi[A] = Multi(values: _*)

  /** (for convenience) calls Single.apply() */
  def apply[A](value: A)(implicit vt: ValueType[A]): Single[A] = Single(value)

  case class Single[A](value: A)(implicit val valueType: ValueType[A]) extends PropertyList[A] {
    def apply(idx: Int): A = if (idx == 0) value else throw new IndexOutOfBoundsException
    override def toString: String = valueType.toString + "Prop(" + value + ")"
    private[this] lazy val bytes = valueType match {
      case ValueType.String => value.asInstanceOf[String].getBytes(asciiEncoding)
      case _ => null
    }
    private[scdbpf] def binaryLength: Int = valueType match {
      case ValueType.String => 13 + bytes.length
      case _ => 9 + valueType.wordLength
    }
    private[scdbpf] def encode(buf: ByteBuffer, id: UInt): Unit = {
      assert(buf.order == LittleEndian)
      buf.putInt(id.toInt)
      buf.putShort(valueType.valueId)
      if (valueType != ValueType.String) {
        buf.putShort(0)
        buf.put(0: Byte)
        valueType.putValue(buf, value)
      } else {
        buf.putShort(0x80)
        buf.put(0: Byte)
        buf.putInt(bytes.length)
        buf.put(bytes)
      }
    }
  }

  case class Multi[A](values: IndexedSeq[A])(implicit val valueType: NumericalValueType[A]) extends PropertyList[A] {
    def apply(idx: Int): A = values(idx)
    override def toString: String = valueType.toString + "PropList(" + values.mkString(", ") + ")"
    private[scdbpf] def binaryLength: Int = 13 + values.length * valueType.wordLength
    private[scdbpf] def encode(buf: ByteBuffer, id: UInt): Unit = {
      assert(buf.order == LittleEndian)
      buf.putInt(id.toInt)
      buf.putShort(valueType.valueId)
      buf.putShort(0x80)
      buf.put(0: Byte)
      buf.putInt(values.length)
      values.foreach(v => valueType.putValue(buf, v))
    }
    def map[B : NumericalValueType](f: A => B) = Multi(values.map(f))
  }

  object Single {
    def unapply[A](prop: PropertyList[A]): Option[A] = prop match {
      case p: Single[_] => Some(p.value)
      case _ => None
    }
  }

  object Multi {
    def apply[A : NumericalValueType](values: A*): Multi[A] = {
      new Multi(values.toIndexedSeq)
    }
    def unapply[A](prop: PropertyList[A]): Option[IndexedSeq[A]] = prop match {
      case p: Multi[_] => Some(p.values)
      case _ => None
    }
  }
}
