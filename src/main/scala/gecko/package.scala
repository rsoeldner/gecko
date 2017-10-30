import cats.MonadError
import cats.evidence.Is

import scala.reflect.ClassTag

package object gecko {

  /** unsafe removal of element at index.
    * No bounds checking.
    *
    */
  @inline protected[gecko] final def removeElemAt[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      arr: Array[A],
      i: Int
  ): Array[A] = {
    val newLen   = arr.length - 1
    val newArray = new Array[A](newLen)
    System.arraycopy(arr, 0, newArray, 0, i)
    System.arraycopy(arr, i + 1, newArray, i, newLen - i)
    newArray
  }

  /** One pass map
    *
    */
  @inline protected[gecko] final def mapCopyArray[
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
  @inline protected[gecko] final def flatMapCopy[
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

  @inline protected[gecko] final def arrayAppend[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      array1: Array[A],
      array2: Array[A]
  ): Array[A] = {
    val newArray = new Array[A](array1.length + array2.length)
    System.arraycopy(array1, 0, newArray, 0, array1.length)
    System.arraycopy(array2, 0, newArray, array1.length, array2.length)
    newArray
  }

  @inline protected[gecko] final def copyRange[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      array1: Array[A],
      begin: Int,
      until: Int
  ): Array[A] = {
    val len      = until - begin
    val newArray = new Array[A](len)
    System.arraycopy(array1, begin, newArray, 0, len)
    newArray
  }

  @inline protected[gecko] final def copyArray[@specialized(Int, Double, Boolean, Long) A: ClassTag](
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

  sealed trait TaggedDataVector {
    type DF[A] <: Array[DataVector[A]]
    def is[A]: Is[Array[DataVector[A]], DF[A]]
  }

  protected val taggedDataVector$$ : TaggedDataVector = new TaggedDataVector {
    type DF[A] = Array[DataVector[A]]
    @inline def is[A]: Is[Array[DataVector[A]], DF[A]] = Is.refl[Array[DataVector[A]]]
  }

  type DataMatrix[A] = taggedDataVector$$.DF[A]

  object DataMatrix {
    def apply[A](vectors: DataVector[A]*): Either[GeckoError, DataMatrix[A]] =
      if (vectors.size <= 0)
        Left(DataframeInitError("No vectors"))
      else {
        if (vectors.forall(_.length == vectors.head.length))
          Right(taggedDataVector$$.is.coerce(vectors.toArray))
        else
          Left(DataframeInitError("Invalid length. DataVectors must all be of the same length"))
      }

    def liftF[F[_], A](vectors: DataVector[A]*)(implicit F: MonadError[F, Throwable]): F[DataMatrix[A]] =
      if (vectors.size <= 0)
        F.raiseError(DataframeInitError("No vectors"))
      else {
        if (vectors.forall(_.length == vectors.head.length))
          F.pure(taggedDataVector$$.is.coerce(vectors.toArray))
        else
          F.raiseError(DataframeInitError("Invalid length. DataVectors must all be of the same length"))
      }

    def fromArray[A](vectors: Array[DataVector[A]]): Either[GeckoError, DataMatrix[A]] =
      if (vectors.length > 0 && vectors.forall(_.length == vectors(0).length))
        Right(is[A].coerce(vectors))
      else
        Left(DataframeInitError("Invalid length. DataVectors must all be of the same length"))

    def fromArrayF[F[_], A](vectors: Array[DataVector[A]])(implicit F: MonadError[F, Throwable]): F[DataMatrix[A]] =
      if (vectors.length > 0 && vectors.forall(_.length == vectors(0).length))
        F.pure(is[A].coerce(vectors))
      else
        F.raiseError(DataframeInitError("Invalid length. DataVectors must all be of the same length"))

    def fromSeq[A](a: Seq[DataVector[A]]): Either[GeckoError, DataMatrix[A]] = apply[A](a: _*)

    def fromSeqF[F[_], A](a: Seq[DataVector[A]])(implicit F: MonadError[F, Throwable]): F[DataMatrix[A]] = liftF(a: _*)

    def unsafeFromArray[A](arr: Array[DataVector[A]]): DataMatrix[A] = is[A].coerce(arr)

    def fill[A](n: Int, elem: DataVector[A]): DataMatrix[A] =
      is.coerce(Array.fill(n)(elem))

    @inline def is[A]: Is[Array[DataVector[A]], DataMatrix[A]] =
      taggedDataVector$$.is[A]
  }

  sealed trait GeckoError extends Exception

  case class DataframeInitError(cause: String) extends GeckoError {
    override def getMessage: String = cause

    override def fillInStackTrace(): Throwable = this
  }

  class DataMatrixSyntax[A](val m: DataMatrix[A]) extends AnyVal {
    def mapDM[B](f: DataVector[A] => DataVector[B]): DataMatrix[B] =
      DataMatrix.is[B].coerce(mapCopyArray(m, f))
  }

}
