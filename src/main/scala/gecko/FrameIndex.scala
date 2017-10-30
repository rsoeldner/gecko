package gecko

import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer
final case class FrameIndex[@specialized(Int, Double, Boolean, Long) A: ClassTag](
    underlying: Array[A],
    private[gecko] val indexes: Array[Int]
) {
  @inline def apply(i: Int): A = underlying(i)

  @inline def index(i: Int): Int = indexes(i)

  @inline def length: Int = underlying.length

  def slice(begin: Int, end: Int): FrameIndex[A] = {
    val underlyingSlice = copyRange(underlying, begin, end)
    val indexSlice = copyRange(indexes, begin, end)
    FrameIndex(underlyingSlice, indexSlice)
  }

  /** Remove elements at index i
    *
    */
  def removeIx(i: Int): FrameIndex[A] =
    if (i < 0 || i >= underlying.length)
      this
    else
      FrameIndex(removeElemAt(underlying, i), removeElemAt(indexes, i))

  def findOne(identifier: A): Int = underlying.indexOf(identifier)

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
