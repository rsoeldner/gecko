package gecko

import cats.Eq

import scala.reflect.ClassTag

sealed abstract class DataVector[@specialized(Int, Double, Boolean, Long) A](
    private[gecko] val underlying: Array[A]
)(implicit emptyGecko: EmptyGecko[A]) {

  def apply(i: Int): A = underlying(i)

  @inline def length: Int = underlying.length

  def map[B: ClassTag](f: A => B): DataVector[B]

  def flatMap[B: ClassTag](f: A => DataVector[B]): DataVector[B]

  def semiFlatMap[B: ClassTag](f: A => Array[B]): DataVector[B]

  def +(other: DataVector[A]): DataVector[A]

  def ++[B >: A: ClassTag](other: DataVector[B]): DataVector[B]

  def drop(n: Int): DataVector[A]

  def dropLast: DataVector[A]
  def dropLastN(n: Int): DataVector[A]

  def slice(begin: Int, end: Int): DataVector[A]

  def head: Option[A]

  def headUnsafe: A

  def last: Option[A]

  def lastUnsafe: A

  def shift(n: Int): DataVector[A]

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

  implicit def eq[@specialized(Int, Double, Boolean, Long) A] = new Eq[DataVector[A]] {
    override def eqv(x: DataVector[A], y: DataVector[A]) = x.underlying.sameElements(y.underlying)
  }

  def apply[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyGecko](values: A*): DataVector[A] =
    fromArray[A](values.toArray)

  final def fromArray[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      array: Array[A]
  )(implicit emptyGecko: EmptyGecko[A]): DataVector[A] =
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

      def dropLast: DataVector[A] =
        fromArray(copyRange(underlying, 0, array.length - 1))

      def dropLastN(n: Int): DataVector[A] =
        fromArray(copyRange(underlying, 0, array.length - n))

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

      def shift(n: Int): DataVector[A] = {
        val len     = underlying.length
        val new_arr = new Array[A](len)
        var i       = 0
        // shift backwards
        if (n < 0) {
          while (i < len + n) { // data part
            new_arr(i) = underlying(i - n)
            i += 1
          }
          while (i < len) { // empty part
            new_arr(i) = emptyGecko.emptyElement
            i += 1
          }
        } else if (n >= 0) {
          while (i < n) { // empty part
            new_arr(i) = emptyGecko.emptyElement
            i += 1
          }
          while (i < len) { // data part
            new_arr(i) = underlying(i - n)
            i += 1
          }
        }
        fromArray(new_arr)
      }
    }

  def empty[@specialized(Int, Double, Boolean, Long) A: ClassTag]: DataVector[A] = fromArray(Array.empty[A])

}
