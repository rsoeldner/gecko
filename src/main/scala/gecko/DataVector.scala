package gecko

import cats.Eq

import scala.reflect.ClassTag

/** An Immutable, array-backed Vector implementation.
  * Insertion is O(n)
  * Removal is O(n)
  * Access is O(1)
  * Replace is O(n)
  *
  */
sealed abstract class DataVector[@specialized(Int, Double, Boolean, Long) A](
    private[gecko] val underlying: Array[A]
)(implicit emptyGecko: EmptyGecko[A]) {

  @inline def apply(i: Int): A = underlying(i)

  def at(i: Int): Option[A] =
    if (0 <= i && i < length && emptyGecko.nonEmpty(apply(i))) {
      Some(apply(i))
    } else None

  /** Number of Elements
    *
    * @return
    */
  @inline def length: Int = underlying.length


  def map[B: ClassTag](f: A => B): DataVector[B]

  def flatMap[B: ClassTag](f: A => DataVector[B]): DataVector[B]

  def semiFlatMap[B: ClassTag](f: A => Array[B]): DataVector[B]

  def replace(i: Int, elem: A): DataVector[A]

  def remove(i: Int): DataVector[A]

  /** Append another datavector of the same type
    *
    */
  def +(other: A): DataVector[A]

  /** Append another datavector, which may be an upcast
    *
    */
  def ++[B >: A: ClassTag](other: DataVector[B]): DataVector[B]

  /** Drop the first n elements
    *
    */
  def drop(n: Int): DataVector[A]

  /** Drop 1, at the end
    *
    */
  def dropLast: DataVector[A] = dropLastN(1)

  /** Drop n elements,
    *
    */
  def dropLastN(n: Int): DataVector[A]

  /** Return a slice of this datavector
    *
    * @return
    */
  def slice(begin: Int, end: Int): DataVector[A]

  /** Return first Element if possible, otherwise None
    *
    * @return
    */
  def head: Option[A]

  /** Return unsafe first element
    *
    * @return
    */
  def headUnsafe: A

  /** Return all elements except the first one
    *
    * @return
    */
  def tail: DataVector[A]

  /** Return last element if possible, otherwise None
    *
    * @return
    */
  def last: Option[A]

  /** Return unsafe last element
    *
    * @return
    */
  def lastUnsafe: A

  /** Shift N elements by shifting the elements inside of the DataVector, and filling the rest
    * of the columns with the `empty` value for A
    * i.e:
      scala> DataVector(1,2,3,4,5).shift(2)
      res2: gecko.DataVector[Int] = DataVector(NA, NA, 1, 2, 3)
    */
  def shift(n: Int): DataVector[A]

  /** Like shift, but instead of filling with the empty value, apply a function F That will apply from the
    * last shifted value, forwards.
    *
    * Useful for, say, moving a sequence up, without losing elements.
    * i.e:
      scala> DataVector(1,2,3,4,5).shiftWithFill(-2, _ + 1)
      res0: gecko.DataVector[Int] = DataVector(3, 4, 5, 6, 7)

      scala> DataVector(1,2,3,4,5).shiftWithFill(3, _ - 1)
      res1: gecko.DataVector[Int] = DataVector(-2, -1, 0, 1, 2)
    */
  def shiftWithFill(shift: Int, f: A => A): DataVector[A]

  // Consider shifting views? Not full priority currently though
//  /** Creates a view into the DataVector containing the shift.
//    * NOTE: Shifting creates an arithmetic differential that
//    * could result in other operations being unsafe
//    *
//    * @param n
//    * @return
//    */
//  def shiftView(n: Int): DataVector[A]

  override def toString: String = {
    val separator = ", "
    var i         = 0
    val builder   = new java.lang.StringBuilder()
    builder.append("DataVector(")

    while (i < length - 1) {
      if (emptyGecko.nonEmpty(apply(i))) {
        builder.append(apply(i))
      } else {
        builder.append("NA")
      }

      builder.append(separator)
      i += 1
    }

    if (emptyGecko.nonEmpty(apply(i))) {
      builder.append(apply(i))
    } else {
      builder.append("NA")
    }

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

  def fromArray[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      array: Array[A]
  )(implicit emptyGecko: EmptyGecko[A]): DataVector[A] =
    new DataVector[A](array) {
      def map[B: ClassTag](f: (A) => B): DataVector[B] = fromArray(mapCopyArray[A, B](underlying, f))

      def flatMap[B: ClassTag](f: (A) => DataVector[B]): DataVector[B] =
        fromArray(flatMapCopy[A, B](underlying, f(_).underlying))

      def semiFlatMap[B: ClassTag](f: (A) => Array[B]): DataVector[B] =
        fromArray(flatMapCopy[A, B](underlying, f))

      def replace(i: Int, elem: A): DataVector[A] =
        if (i >= length || i < 0)
          this
        else {
          val newArray = copyArray(underlying)
          newArray(i) = elem
          fromArray(newArray)
        }

      def remove(i: Int): DataVector[A] =
        if (i >= length || i < 0)
          this
        else {
          fromArray(removeElemAt(underlying, i))
        }

      def +(other: A): DataVector[A] = {
        val len      = length
        val newArray = new Array[A](len + 1)
        System.arraycopy(underlying, 0, newArray, 0, len)
        newArray(len) = other
        fromArray(newArray)
      }

      def ++[B >: A: ClassTag](other: DataVector[B]): DataVector[B] =
        fromArray[B](arrayAppend[B](underlying.asInstanceOf[Array[B]], other.underlying))

      def drop(n: Int): DataVector[A] =
        fromArray(copyRange(underlying, n, underlying.length))

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
        if (n < 0 && n > -len) {
          while (i < len + n) { // data part
            new_arr(i) = apply(i - n)
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
            new_arr(i) = apply(i - n)
            i += 1
          }
        }
        fromArray(new_arr)
      }

      def shiftWithFill(n: Int, f: (A) => A): DataVector[A] = {
        val len     = length
        val new_arr = new Array[A](len)
        var i       = 0
        // shift backwards
        if (n < 0) {

          while (i < len + n) { //Shift portion
            new_arr(i) = apply(i - n)
            i += 1
          }
          while (i < len) { //fill portion
            new_arr(i) = f(new_arr(i - 1))
            i += 1
          }
        } else if (n >= 0 && n < len) {
          i = len - 1
          while (i > n - 1) { // empty part
            new_arr(i) = apply(i - n)
            i -= 1
          }
          while (i >= 0) { // data part
            new_arr(i) = f(new_arr(i + 1))
            i -= 1
          }
        } else {
          return this
        }
        fromArray(new_arr)
      }

      def tail: DataVector[A] =
        if (length <= 0)
          DataVector.empty[A]
        else
          slice(1, length)
    }

  def empty[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyGecko]: DataVector[A] = fromArray(Array.empty[A])

  def fillEmpty[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      len: Int
  )(implicit emptyGecko: EmptyGecko[A]): DataVector[A] =
    fromArray(Array.fill(len)(emptyGecko.emptyElement))

}
