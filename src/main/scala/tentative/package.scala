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
  ](arr: Array[A], b: A => B): Array[B] = {
    val arrayLen = arr.length
    val newArray = new Array[B](arrayLen)
    var i        = 0
    while (i < arrayLen) {
      newArray(i) = b(arr(i))
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
  ](arr: Array[A], f: A => Array[B]): Array[B] = {
    val arrayLen         = arr.length
    var finalLen         = 0
    val newArrays        = new Array[Array[B]](arrayLen)
    var i                = 0
    var newArr: Array[B] = null
    while (i < arrayLen) {
      newArr = f(arr(i))
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

}
