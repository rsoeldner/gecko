import scala.reflect.ClassTag
import sun.misc.Unsafe
import java.lang.reflect.Field

package object tentative {

//  val f: Field = classOf[Unsafe].getDeclaredField("theUnsafe")
//  f.setAccessible(true)
//  private [tentative] val unsafe = f.get(null).asInstanceOf[Unsafe]
  // Consider sun misc unsafe for array alloc? :D

  /** One pass map
    *
    */
  @inline protected[tentative] final def mapCopyArray[
      @specialized(Int, Double, Boolean, Long) A,
      @specialized(Int, Double, Boolean, Long) B: ClassTag
  ](arr: Array[A], b: A => B)(implicit empty1: EmptyGecko[A], empty2: EmptyGecko[B]): Array[B] = {
    val arrayLen = arr.length
    val newArray = new Array[B](arrayLen)
    var i        = 0
    var elem     = empty1.emptyElement
    while (i < arrayLen) {
      elem = arr(i)
      if (empty1.nonEmpty(elem))
        newArray(i) = b(arr(i))
      else
        newArray(i) = empty2.emptyElement
      i += 1
    }
    newArray
  }

  /** A linear time, 2 pass flatmap on arrays
    *
    */
  @inline protected[tentative] final def flatMapCopy[
      @specialized(Int, Double, Boolean, Long) A,
      @specialized(Int, Double, Boolean, Long) B: ClassTag
  ](arr: Array[A], f: A => Array[B])(implicit empty1: EmptyGecko[A], empty2: EmptyGecko[B]): Array[B] = {
    val arrayLen         = arr.length
    var finalLen         = 0
    val newArrays        = new Array[Array[B]](arrayLen)
    var i                = 0
    var newArr: Array[B] = null
    var elem             = empty1.emptyElement
    while (i < arrayLen) {
      elem = arr(i)
      if (empty1.nonEmpty(elem))
        newArr = f(arr(i))
      else
        newArr = Array(empty2.emptyElement)

      finalLen += newArr.length
      newArrays(i) = newArr
      i += 1
    }
    val finalArr = new Array[B](finalLen)
    i = 0
    var currLen = 0
    while (i < finalLen) {
      System.arraycopy(newArrays(i), 0, finalArr, currLen, newArrays(i).length)
      currLen += newArrays(i).length
      i += 1
    }
    finalArr
  }

  @inline protected[tentative] final def arrayAppend[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      array1: Array[A],
      array2: Array[A]
  ): Array[A] = {
    val newArray = new Array[A](array1.length + array2.length)
    System.arraycopy(array1, 0, newArray, 0, array1.length)
    System.arraycopy(array2, 0, newArray, array1.length, array2.length)
    newArray
  }

  @inline protected[tentative] final def copyRange[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      array1: Array[A],
      begin: Int,
      until: Int
  ): Array[A] = {
    val len      = until - begin
    val newArray = new Array[A](len)
    System.arraycopy(array1, begin, newArray, 0, len)
    newArray
  }

  @inline protected[tentative] final def copyArray[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      array1: Array[A]
  ): Array[A] = {
    val newArray = new Array[A](array1.length)
    System.arraycopy(array1, 0, newArray, 0, array1.length)
    newArray
  }

  sealed trait EmptyGecko[A] {
    val emptyElement: A
    def nonEmpty(a: A): Boolean
  }

  implicit val geckoInt: EmptyGecko[Int] = new EmptyGecko[Int] {
    val emptyElement: Int = Int.MinValue

    def nonEmpty(a: Int): Boolean = a != Int.MinValue
  }

  implicit val geckoLong: EmptyGecko[Long] = new EmptyGecko[Long] {
    val emptyElement: Long = Long.MinValue

    def nonEmpty(a: Long): Boolean = a != Long.MinValue
  }

  implicit val geckoDouble: EmptyGecko[Double] = new EmptyGecko[Double] {
    val emptyElement: Double = Double.NaN

    def nonEmpty(a: Double): Boolean = a != Double.NaN
  }

  implicit val geckoBool: EmptyGecko[Boolean] = new EmptyGecko[Boolean] {
    val emptyElement: Boolean = false

    def nonEmpty(a: Boolean): Boolean = a
  }

  implicit val emptyGeckoAny: EmptyGecko[Any] = new EmptyGecko[Any] {
    val emptyElement: Any = null

    def nonEmpty(a: Any): Boolean = a != null
  }

  implicit final def emptyGeckerino[A]: EmptyGecko[A] = emptyGeckoAny.asInstanceOf[EmptyGecko[A]]

}
