package gecko

import scala.reflect.ClassTag

final case class FrameIndex[@specialized(Int, Double, Boolean, Long) A: ClassTag](underlying: Array[A]) {

  @inline private[gecko] def apply(i: Int): A = underlying(i)

  /** Return length
    *
    * @return
    */
  @inline def length: Int = underlying.length

  /** Unsafe versionm return sliced frame
    *
    * @param begin start index
    * @param end end index (exclusive)
    * @return
    */
  def unsafeSlice(begin: Int, end: Int): FrameIndex[A] = {
    val underlyingSlice = copyRange(underlying, begin, end)
    FrameIndex(underlyingSlice)
  }

  /** Slice specific range
    *
    * @param begin start index (inclusive)
    * @param end end index (exclusive)
    * @return
    */
  def slice(begin: Int, end: Int): Either[GeckoError, FrameIndex[A]] =
    if (0 <= begin && begin < end && end < length) Right(unsafeSlice(begin, end))
    else Left(InvalidArgumentError)

  /** concat FrameIndex[A]
    *
    * @param other Other frame to concat
    * @return
    */
  def ++(other: FrameIndex[A]) = {
    val size   = length + other.length
    val values = Array.concat(underlying, other.underlying)
    FrameIndex(values)
  }

  /** Alias for ++
    *
    * @param other
    * @return
    */
  def concat(other: FrameIndex[A]) = this ++ other

  /** Unsafe version, drop element at index
    *
    * @param i index to drop
    * @return
    */
  def unsafeDropIx(i: Int): FrameIndex[A] = FrameIndex(removeElemAt(underlying, i))

  /** Drop element at index
    *
    * @param i index to drop
    * @return
    */
  def dropIx(i: Int): Either[GeckoError, FrameIndex[A]] =
    if (0 <= i && i < length) {
      if(length == 1) Right(FrameIndex.empty)
      else Right(unsafeDropIx(i))
    }
    else Left(IndexOutOfBoundError(i, length))

  /** Finds first occurrence
    *
    * @param identifier to search for
    * @return
    */
  def findOne(identifier: A): Either[GeckoError, Int] = {
    val pos = unsafeFindOne(identifier)
    if (0 <= pos) Right(pos)
    else Left(ElementNotFoundError(identifier))
  }

  /** Unsafe version, see findOne
    *
    * @param identifier
    * @return
    */
  def unsafeFindOne(identifier: A): Int = underlying.indexOf(identifier)

  /** Find oll occurrences
    *
    * @param identifier to search for
    * @return
    */
  def findAll(identifier: A): IndexedSeq[Int] = (0 until length).filter(underlying(_) == identifier)
}

object FrameIndex {
  def default(size: Int): FrameIndex[Int] = FrameIndex(Array.range(0, size))

  def fromSeq[@specialized(Int, Double, Boolean, Long) C: ClassTag](c: Seq[C]): FrameIndex[C] = FrameIndex[C](c.toArray)

  def empty[A: ClassTag]: FrameIndex[A] = FrameIndex(Array.empty[A])
}
