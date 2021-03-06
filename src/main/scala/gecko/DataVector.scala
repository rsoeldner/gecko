package gecko

import cats.Eq
import gecko.DataVector.fromArray

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

/** An Immutable, array-backed Vector implementation.
  * Insertion is O(n)
  * Removal is O(n)
  * Access is O(1)
  * Replace is O(n)
  *
  */
sealed abstract class DataVector[@specialized(Int, Double, Boolean, Long) A: ClassTag](
    private[gecko] val underlying: Array[A]
)(implicit emptyGecko: EmptyGecko[A]) {

  @inline private[gecko] def apply(i: Int): A = underlying(i)

  /** Return value at index
    *
    * @param i index
    * @return
    */
  def at(i: Int): Option[A] =
    if (0 <= i && i < length && emptyGecko.nonEmpty(apply(i))) {
      Some(apply(i))
    } else None

  /** Unsafe version, return value at index
    *
    * @param i index
    * @return
    */
  def unsafeAt(i: Int) = apply(i)

  /** Number of Elements
    *
    * @return
    */
  @inline def length: Int = underlying.length

  /** map f over each element
    *
    * @param f
    * @tparam B
    * @return
    */
  def map[B: ClassTag: EmptyGecko](f: A => B): DataVector[B] = fromArray(mapCopyArray[A, B](underlying, f))

  /** flatMap over each element
    *
    * @param f
    * @tparam B
    * @return
    */
  def flatMap[B: ClassTag: EmptyGecko](f: A => DataVector[B]): DataVector[B] =
    fromArray(flatMapCopy[A, B](underlying, f(_).underlying))

  /** semiFlatMap
    *
    * @param f
    * @tparam B
    * @return
    */
  def semiFlatMap[B: ClassTag: EmptyGecko](f: A => Array[B]): DataVector[B] = fromArray(flatMapCopy[A, B](underlying, f))

  /** Replace value at specific index
    *
    * @param i    index
    * @param elem new value
    * @return
    */
  def replace(i: Int, elem: A): Either[GeckoError, DataVector[A]] =
    if (0 <= i && i < length) Right(unsafeReplace(i, elem))
    else Left(IndexOutOfBoundError(i, length))

  /** Unsafe version, replace value at specific index
    *
    * @param i    index
    * @param elem new value
    * @return
    */
  def unsafeReplace(i: Int, elem: A): DataVector[A] = {
    val newArray = copyArray(underlying)
    newArray(i) = elem
    fromArray(newArray)
  }

  /** Remove element at specific index
    *
    * @param i index
    * @return
    */
  def remove(i: Int): Either[GeckoError, DataVector[A]] =
    if (0 <= i && i < length) Right(unsafeRemove(i))
    else Left(IndexOutOfBoundError(i, length))

  /** Unsafe version, remove element at specific index
    *
    * @param i index
    * @return
    */
  def unsafeRemove(i: Int): DataVector[A] = fromArray(removeElemAt(underlying, i))

  /** Append element at the end
    *
    * @param other new element
    * @return
    */
  def +(other: A): DataVector[A] = {
    val len      = length
    val newArray = new Array[A](len + 1)
    System.arraycopy(underlying, 0, newArray, 0, len)
    newArray(len) = other
    fromArray(newArray)
  }

  /** Append another datavector, which may be an upcast
    *
    */
  def ++[B >: A: ClassTag: EmptyGecko](other: DataVector[B]): DataVector[B] =
    fromArray[B](arrayAppend[B](underlying.asInstanceOf[Array[B]], other.underlying))

  /** Drop value at specific index
    *
    * @param n index
    * @return
    */
  def drop(n: Int): Either[GeckoError, DataVector[A]] =
    if (0 <= n && n < length) Right(unsafeDrop(n))
    else Left(IndexOutOfBoundError(n, length))

  /** Unsafe version, drop value at specific index
    *
    * @param n
    * @return
    */
  def unsafeDrop(n: Int): DataVector[A] = fromArray(copyRange(underlying, n, underlying.length))

  /** Drop the last value
    *
    * @return
    */
  def dropLast: Either[GeckoError, DataVector[A]] = dropLastN(1)

  /** Unsafe version, drop the last value
    *
    * @return
    */
  def unsafeDropLast: DataVector[A] = unsafeDropLastN(1)

  /** Drop the last n values
    *
    * @param n number of elements
    * @return
    */
  def dropLastN(n: Int): Either[GeckoError, DataVector[A]] =
    if (0 <= n && n < length) Right(unsafeDropLastN(n))
    else Left(NotEnoughElementsError(n, length))

  /** Unsafe version, drop the last n elements
    *
    * @param n number of elements
    * @return
    */
  def unsafeDropLastN(n: Int): DataVector[A] = fromArray(copyRange(underlying, 0, underlying.length - n))

  /** Return a slice of this datavector
    *
    * @return
    */
  def slice(begin: Int, end: Int): Either[GeckoError, DataVector[A]] =
    if (0 <= begin && begin < end && end < length) Right(unsafeSlice(begin, end))
    else Left(InvalidArgumentError)

  /** Unsafe version, return a slice of this datavector
    *
    * @param begin
    * @param end
    * @return
    */
  def unsafeSlice(begin: Int, end: Int): DataVector[A] = fromArray(copyRange(underlying, begin, end))

  /** Return first Element if possible, otherwise None
    *
    * @return
    */
  def head: Option[A] =
    if (underlying.length == 0)
      None
    else
      Some(unsafeHead)

  /** Return unsafe first element
    *
    * @return
    */
  def unsafeHead: A = underlying(0)

  /** Return all elements except the first one
    *
    * @return
    */
  def tail: Either[GeckoError, DataVector[A]] = slice(1, length)

  /** Unsafe version, return the all elements except the first one
    *
    * @return
    */
  def unsafeTail: DataVector[A] = unsafeSlice(1, length)

  /** Return last element if possible, otherwise None
    *
    * @return
    */
  def last: Option[A] =
    if (underlying.length == 0)
      None
    else
      Some(unsafeLast)

  /** Return unsafe last element
    *
    * @return
    */
  def unsafeLast: A = underlying(underlying.length - 1)

  /** Filter values that match a predicate
    *
    * @param p predicate
    * @return
    */
  def filter(p: A => Boolean): DataVector[A] = {
    val buf = new ListBuffer[A]
    var i   = 0
    while (i < length) {
      if (emptyGecko.nonEmpty(underlying(i)) && p(underlying(i))) {
        buf += underlying(i)

        i += 1
      }
    }
    fromArray(buf.toArray)
  }

  /** Zip with another DataVector
    *
    * @param other
    * @tparam B
    * @return
    */
  def zip[B >: A: ClassTag: EmptyGecko](other: DataVector[B]): DataVector[(A, B)] =
    fromArray(underlying.zip(other.underlying))

  /** TakeWhile value matches predicate
    *
    * @param p  predicate
    * @param ix starting index
    * @return
    */
  def takeWhile(p: A => Boolean, ix: Int = 0): DataVector[A] = {
    var i = ix
    while (i < length) {
      if (!(emptyGecko.nonEmpty(underlying(i)) && p(underlying(i))))
        return fromArray(copyRange[A](underlying, ix, i))

      i += 1
    }
    this
  }

  /** Shift N elements by shifting the elements inside of the DataVector, and filling the rest
    * of the columns with the `empty` value for A
    * i.e:
    * scala> DataVector(1,2,3,4,5).shift(2)
    * res2: gecko.DataVector[Int] = DataVector(NA, NA, 1, 2, 3)
    */
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

  /** Like shift, but instead of filling with the empty value, apply a function F That will apply from the
    * last shifted value, forwards.
    *
    * Useful for, say, moving a sequence up, without losing elements.
    * i.e:
    * scala> DataVector(1,2,3,4,5).shiftWithFill(-2, _ + 1)
    * res0: gecko.DataVector[Int] = DataVector(3, 4, 5, 6, 7)
    * *
    * scala> DataVector(1,2,3,4,5).shiftWithFill(3, _ - 1)
    * res1: gecko.DataVector[Int] = DataVector(-2, -1, 0, 1, 2)
    */
  def shiftWithFill(n: Int, f: A => A): DataVector[A] = {
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

  override def equals(obj: scala.Any): Boolean = obj match {
    case d: DataVector[_] =>
      underlying.sameElements(d.underlying)
    case _ => false
  }
}

object DataVector {

  implicit def eq[@specialized(Int, Double, Boolean, Long) A] = new Eq[DataVector[A]] {
    override def eqv(x: DataVector[A], y: DataVector[A]) = x.underlying.sameElements(y.underlying)
  }

  def apply[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyGecko](values: A*): DataVector[A] =
    fromArray[A](values.toArray)

  def fromArray[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyGecko](
      array: Array[A]
  ): DataVector[A] = new DataVector[A](array) {}

  def empty[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyGecko]: DataVector[A] = fromArray(Array.empty[A])

  def fillEmpty[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      len: Int
  )(implicit emptyGecko: EmptyGecko[A]): DataVector[A] =
    fromArray(Array.fill(len)(emptyGecko.emptyElement))

}
