package tentative

import scala.reflect.ClassTag

sealed abstract class DataVector[@specialized(Int, Double, Boolean, Long) A](
    private[tentative] val underlying: Array[A]
) {

  def apply(i: Int): A = underlying(i)

  def map[B: ClassTag](f: A => B): DataVector[B]

  def flatMap[B: ClassTag](f: A => DataVector[B]): DataVector[B]

  def semiFlatMap[B: ClassTag](f: A => Array[B]): DataVector[B]

  def +(other: DataVector[A]): DataVector[A]

  def ++[B >: A: ClassTag](other: DataVector[B]): DataVector[B]

  def drop(n: Int): DataVector[A]

  def dropLast(n: Int): DataVector[A]

  def slice(begin: Int, end: Int): DataVector[A]

  def head: Option[A]

  def headUnsafe: A

  def last: Option[A]

  def lastUnsafe: A



  //def shift(n: Int)

  //def shiftWithF(n: Int, transform: T => T): Vec[T] = {

  //def pad: Vec[T] = VecImpl.pad(this)(scalarTag)
  override def toString: String = {
    val builder = new java.lang.StringBuilder()
    builder.append("DataVector(")
    builder.append(mapCopyArray[A, String](underlying, _.toString).mkString(", "))
    builder.append(")")
    builder.toString
  }
}

object DataVector {

  def apply[@specialized(Int, Double, Boolean, Long) A: ClassTag](values: A*): DataVector[A] =
    fromArray[A](values.toArray)

  final def fromArray[@specialized(Int, Double, Boolean, Long) A: ClassTag](array: Array[A]): DataVector[A] =
    new DataVector[A](array) {
      def map[B: ClassTag](f: (A) => B): DataVector[B] = fromArray(mapCopyArray[A, B](underlying, f))

      def flatMap[B: ClassTag](f: (A) => DataVector[B]): DataVector[B] =
        fromArray(flatMapCopy[A, B](underlying, f(_).underlying))

      def semiFlatMap[B: ClassTag](f: (A) => Array[B]): DataVector[B] =
        fromArray(flatMapCopy[A, B](underlying, f))

      def +(other: DataVector[A]): DataVector[A] =
        fromArray(arrayAppend[A](underlying, other.underlying))

      def ++[B >: A: ClassTag](other: DataVector[B]): DataVector[B] =
        fromArray[B](arrayAppend[B](underlying.asInstanceOf[Array[B]], other.underlying))

      def drop(n: Int): DataVector[A] =
        fromArray(copyRange(underlying, n, underlying.length))

      def dropLast(n: Int): DataVector[A] =
        fromArray(copyRange(underlying, 0, array.length - 1))

      def slice(begin: Int, until: Int): DataVector[A] =
        fromArray(copyRange(underlying, begin, until))

      def head: Option[A] =
        if (underlying.length == 0)
          None
        else
          Some(underlying(0))

      def headUnsafe: A = underlying(0)

      def last: Option[A] =
        if (underlying.length == 0)
          None
        else
          Some(underlying(underlying.length - 1))

      def lastUnsafe: A = underlying(underlying.length - 1)
    }

  def empty[@specialized(Int, Double, Boolean, Long) A: ClassTag]: DataVector[A] = fromArray(Array.empty[A])

}
