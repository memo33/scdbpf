package scdbpf

import java.nio.ByteBuffer
import java.nio.ByteOrder.{LITTLE_ENDIAN => LittleEndian}
import passera.unsigned._
import scala.reflect.ClassTag
import scala.collection.mutable.{Builder, ArrayBuilder}
import scala.collection.generic.CanBuildFrom
import DbpfProperty.ValueType
import DbpfUtil.asciiEncoding


object DbpfProperty {

  sealed trait PropertyList[A] {
    val valueType: ValueType[A]
    def apply(idx: Int): A
    private[scdbpf] def binaryLength: Int
    private[scdbpf] def encode(buf: ByteBuffer, id: UInt): Unit
  }

  type PropList = PropertyList[_]
  type Property = (UInt, PropList)
  type ValueType[D] = ValueType.ValueType[D]

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
          val values: Array[A] = Array.fill(cnt)(getValue(buf))
          new Multi(this, values)
        case None =>
          new SingleImpl(this, getValue(buf))
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
        new StringProperty(new String(bytes, asciiEncoding))
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
  def apply[A](values: Seq[A])(implicit vt: ValueType[A] with ValueType.Numerical[A]): Multi[A] = Multi(values: _*)

  /** (for convenience) calls Single.apply() */
  def apply[A](value: A)(implicit vt: ValueType[A]): Single[A] = Single(value)

  sealed trait Single[A] extends PropertyList[A] {

    val value: A
    def apply(idx: Int): A = if (idx == 0) value else throw new IndexOutOfBoundsException
    def copy(value: A): Single[A]

    final override def equals(obj: Any): Boolean = obj match {
      case that: Single[_] => this.valueType == that.valueType && this.value == that.value
      case _ => false
    }

    override def toString: String = valueType.toString + "Prop(" + value + ")"
  }

  object Single {

    def apply[A](value: A)(implicit vt: ValueType[A]): Single[A] = vt match {
      case ValueType.String => (new StringProperty(value.asInstanceOf[String])).asInstanceOf[Single[A]]
      case vt: ValueType.Numerical[_] => new SingleImpl(vt.asInstanceOf[ValueType[A] with ValueType.Numerical[A]], value)
      case x if x == null => throw new IllegalArgumentException
    }
    def unapply[A](prop: PropertyList[A]): Option[Single[A]] = prop match {
      case p: Single[_] => Some(p)
      case _ => None
    }
  }

  private final class SingleImpl[A](val valueType: ValueType[A] with ValueType.Numerical[A], val value: A) extends Single[A] {

    private[scdbpf] lazy val binaryLength: Int = 9 + valueType.wordLength
    def copy(value: A): SingleImpl[A] = new SingleImpl(valueType, value)

    private[scdbpf] def encode(buf: ByteBuffer, id: UInt): Unit = {
      assert(buf.order == LittleEndian)
      buf.putInt(id.toInt)
      buf.putShort(valueType.valueId)
      buf.putShort(0)
      buf.put(0: Byte)
      valueType.putValue(buf, value)
    }
  }

  private[DbpfProperty] sealed trait MultiLike[A] extends PropertyList[A] {

    protected def encValues(buf: ByteBuffer): Unit

    private[scdbpf] def encode(buf: ByteBuffer, id: UInt): Unit = {
      assert(buf.order == LittleEndian)
      buf.putInt(id.toInt)
      buf.putShort(valueType.valueId)
      buf.putShort(0x80)
      buf.put(0: Byte)
      encValues(buf)
    }
  }

  private final class StringProperty(val value: String) extends Single[String] with MultiLike[String] {
    val valueType = ValueType.String

    private[this] lazy val bytes = value.getBytes(asciiEncoding)
    private[scdbpf] lazy val binaryLength: Int = 13 + bytes.length

    protected def encValues(buf: ByteBuffer): Unit = {
      buf.putInt(bytes.length)
      buf.put(bytes)
    }

    def copy(value: String): StringProperty = new StringProperty(value)
  }

  final class Multi[A : ClassTag] private[DbpfProperty] (val valueType: ValueType[A] with ValueType.Numerical[A], values: Array[A])
      extends MultiLike[A]
      with IndexedSeq[A] with scala.collection.IndexedSeqOptimized[A, Multi[A]] {

    private[scdbpf] lazy val binaryLength: Int = 13 + this.length * valueType.wordLength

    protected def encValues(buf: ByteBuffer): Unit = {
      buf.putInt(length)
      this foreach (v => valueType.putValue(buf, v))
    }

    final override def equals(obj: Any): Boolean = obj match {
      case that: Multi[_] => this.valueType == that.valueType && super.equals(that)
      case _ => false
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Multi[_]]

    override protected[this] def newBuilder: Builder[A, Multi[A]] = Multi.newBuilder(implicitly[ClassTag[A]], valueType)

    def apply(idx: Int): A = values(idx)
    val length: Int = values.length

    override def stringPrefix: String = valueType.toString + "PropList"
  }

  object Multi {

    def apply[A](values: A*)(implicit vt: ValueType[A] with ValueType.Numerical[A]): Multi[A] = {
      new Multi(vt, values.toArray(vt.tag))(vt.tag)
    }
    def unapply[A](prop: PropertyList[A]): Option[Multi[A]] = prop match {
      case p: Multi[_] => Some(p)
      case _ => None
    }

    def newBuilder[A](implicit ev: ClassTag[A], vt: ValueType[A] with ValueType.Numerical[A]): Builder[A, Multi[A]] =
      ArrayBuilder.make[A]() mapResult { arr: Array[A] => new Multi(vt, arr) }

    implicit def cbf[C](implicit ev: ClassTag[C], vt: ValueType[C] with ValueType.Numerical[C]) = new CanBuildFrom[Multi[_], C, Multi[C]] {
      def apply(): Builder[C, Multi[C]] = newBuilder
      def apply(from: Multi[_]) = newBuilder
    }
  }
}
