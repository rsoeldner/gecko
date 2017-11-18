package gecko

import scala.reflect.ClassTag

final case class FrameIndex[@specialized(Int, Double, Boolean, Long) A: ClassTag](underlying: Array[A]) {

  @inline def apply(i: Int): A = underlying(i)

  @inline def length: Int = underlying.length

  def unsafeSlice(begin: Int, end: Int): FrameIndex[A] = {
    val underlyingSlice = copyRange(underlying, begin, end)
    FrameIndex(underlyingSlice)
  }

  def slice(begin: Int, end: Int): Either[GeckoError, FrameIndex[A]] =
    if (0 <= begin && begin < end && end < length) Right(unsafeSlice(begin, end))
    else Left(InvalidArgumentError)

  def ++(other: FrameIndex[A]) = {
    val size   = length + other.length
    val values = Array.concat(underlying, other.underlying)
    FrameIndex(values)
  }

  /** Remove elements at index i
    *
    */
  def unsafeDropIx(i: Int): FrameIndex[A] = FrameIndex(removeElemAt(underlying, i))

  def dropIx(i: Int): Either[GeckoError, FrameIndex[A]] =
    if (0 <= i && i < length) Right(unsafeDropIx(i))
    else Left(IndexOutOfBoundError(i, length))

  def findOne(identifier: A): Either[GeckoError, Int] = {
    val pos = unsafeFindOne(identifier)
    if (0 <= pos) Right(pos)
    else Left(ElementNotFoundError(identifier))
  }

  def unsafeFindOne(identifier: A): Int = underlying.indexOf(identifier)

  def findAll(identifier: A): IndexedSeq[Int] = (0 until length).filter(underlying(_) == identifier)
}
object FrameIndex {
  def default(size: Int): FrameIndex[Int] = {
    val ix = Array.range(0, size)
    FrameIndex(ix)
  }

  def fromSeq[C: ClassTag](c: Seq[C]): FrameIndex[C] = {
    val ix = Array.range(0, c.length)
    FrameIndex[C](c.toArray)
  }

  def empty[A: ClassTag]: FrameIndex[A] =
    FrameIndex(Array.empty[A])
}
