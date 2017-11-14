package gecko

import scala.reflect.ClassTag

final case class FrameIndex[@specialized(Int, Double, Boolean, Long) A: ClassTag](
    underlying: Array[A],
    private[gecko] val indexes: Array[Int]
) {
  @inline def apply(i: Int) = at(i)

  @inline def length: Int = underlying.length

  def unsafeAt(i: Int): A = underlying(i)

  def at(i: Int): Either[GeckoError, A] =
    if(0 <= i && i < length) Right(unsafeAt(i))
    else Left(NotEnoughElementsError(i, length))

  def unsafeIndex(i: Int): Int = indexes(i)

  def index(i: Int): Either[GeckoError, Int] =
    if(0 <= i && i < length) Right(unsafeIndex(i))
    else Left(NotEnoughElementsError(i, length))


  def unsafeSlice(begin: Int, end: Int): FrameIndex[A] = {
    val underlyingSlice = copyRange(underlying, begin, end)
    val indexSlice = copyRange(indexes, begin, end)
    FrameIndex(underlyingSlice, indexSlice)
  }

  def slice(begin: Int, end: Int): Either[GeckoError, FrameIndex[A]] =
    if(0 <= begin && begin < end && end < length) Right(unsafeSlice(begin, end))
    else Left(InvalidArgumentError)


  def ++(other: FrameIndex[A]) = {
    val size = length + other.length
    val ix = Array.range(0, size)
    val values = Array.concat(underlying, other.underlying)
    FrameIndex(values, ix)
  }

  /** Remove elements at index i
    *
    */
  def unsafeRemoveIx(i: Int): FrameIndex[A] = FrameIndex(removeElemAt(underlying, i), removeElemAt(indexes, i))

  def removeIx(i: Int): Either[GeckoError, FrameIndex[A]] =
    if(0 <= i && i < length) Right(unsafeRemoveIx(i))
    else Left(IndexOutOfBoundError(i, length))

  def findOne(identifier: A): Either[GeckoError, Int] = {
    val pos = unsafeFindOne(identifier)
    if(0 <= pos) Right(pos)
    else Left(ElementNotFoundError(identifier))
  }

  def unsafeFindOne(identifier: A): Int = underlying.indexOf(identifier)

  def findAll(identifier: A): Array[Int] = indexes.filter(underlying(_) == identifier)
}

object FrameIndex {

  def default(size: Int): FrameIndex[Int] = {
    val ix = Array.range(0, size)
    FrameIndex(ix, ix)
  }
  def empty[A: ClassTag]: FrameIndex[A] =
    FrameIndex(Array.empty[A], Array.empty[Int])
}
