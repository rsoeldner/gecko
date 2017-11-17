import cats.MonadError
import cats.evidence.Is
import scala.reflect.ClassTag

package object gecko extends EmptyPrintInstances with EmptyGeckoInstances {

  /** unsafe removal of element at index.
    * No bounds checking.
    *
    */
  @inline protected[gecko] final def removeElemAt[@specialized(Int, Double, Boolean, Long) A: ClassTag](
                                                                                                         arr: Array[A],
                                                                                                         i: Int
                                                                                                       ): Array[A] = {
    val newLen = arr.length - 1
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
    var i = 0
    var elem = empty1.emptyElement
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
    val arrayLen = arr.length
    var finalLen = 0
    val newArrays = new Array[Array[B]](arrayLen)
    var i = 0
    var newArr: Array[B] = null
    var elem = empty1.emptyElement
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
    val len = until - begin
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

  sealed trait TaggedDataVector {
    type DF[A] <: Array[DataVector[A]]

    def is[A]: Is[Array[DataVector[A]], DF[A]]
  }

  protected val taggedDataVector$$: TaggedDataVector = new TaggedDataVector {
    type DF[A] = Array[DataVector[A]]

    @inline def is[A]: Is[Array[DataVector[A]], DF[A]] = Is.refl[Array[DataVector[A]]]
  }

  type DataMatrix[A] = taggedDataVector$$.DF[A]

  object DataMatrix {
    def apply[A](vectors: DataVector[A]*): Either[GeckoError, DataMatrix[A]] =
      if (vectors.size <= 0)
        Left(DataFrameInitError("No vectors"))
      else {
        if (vectors.forall(_.length == vectors.head.length))
          Right(taggedDataVector$$.is.coerce(vectors.toArray))
        else
          Left(DataFrameInitError("Invalid length. DataVectors must all be of the same length"))
      }

    def fromArrayWithDim[A: ClassTag](rows: Int, cols: Int, values: Array[A]): Either[GeckoError, DataMatrix[A]] = {
      val n = rows * cols
      if (n != values.length)
        Left(DataFrameInitError(s"$rows * $cols != ${values.length}"))
      else {
        val newArray = new Array[DataVector[A]](rows)
        var r = 0

        while (r < rows) {
          val startPos = r * cols
          val endPos = startPos + cols
          newArray(r) = DataVector.fromArray(copyRange(values, startPos, endPos))

          r += 1
        }
        DataMatrix.fromArray(newArray)
      }
    }

    def liftF[F[_], A](vectors: DataVector[A]*)(implicit F: MonadError[F, Throwable]): F[DataMatrix[A]] =
      if (vectors.size <= 0)
        F.raiseError(DataFrameInitError("No vectors"))
      else {
        if (vectors.forall(_.length == vectors.head.length))
          F.pure(taggedDataVector$$.is.coerce(vectors.toArray))
        else
          F.raiseError(DataFrameInitError("Invalid length. DataVectors must all be of the same length"))
      }

    def fromArray[A](vectors: Array[DataVector[A]]): Either[GeckoError, DataMatrix[A]] =
      if (vectors.length > 0 && vectors.forall(_.length == vectors(0).length))
        Right(is[A].coerce(vectors))
      else
        Left(DataFrameInitError("Invalid length. DataVectors must all be of the same length"))

    def fromArrayF[F[_], A](vectors: Array[DataVector[A]])(implicit F: MonadError[F, Throwable]): F[DataMatrix[A]] =
      if (vectors.length > 0 && vectors.forall(_.length == vectors(0).length))
        F.pure(is[A].coerce(vectors))
      else
        F.raiseError(DataFrameInitError("Invalid length. DataVectors must all be of the same length"))

    def fromSeq[A](a: Seq[DataVector[A]]): Either[GeckoError, DataMatrix[A]] = apply[A](a: _*)

    def unsafeFromSeq[A](a: Seq[DataVector[A]]): DataMatrix[A] = is.coerce(a.toArray)

    def fromSeqF[F[_], A](a: Seq[DataVector[A]])(implicit F: MonadError[F, Throwable]): F[DataMatrix[A]] = liftF(a: _*)

    def unsafeFromArray[A](arr: Array[DataVector[A]]): DataMatrix[A] = is[A].coerce(arr)

    def fill[A](n: Int, elem: DataVector[A]): DataMatrix[A] =
      is.coerce(Array.fill(n)(elem))

    @inline def is[A]: Is[Array[DataVector[A]], DataMatrix[A]] =
      taggedDataVector$$.is[A]

    def empty[A: ClassTag]: DataMatrix[A] = is[A].coerce(Array.empty[DataVector[A]])
  }

  implicit class DataMatrixSyntax[A](val m: DataMatrix[A]) extends AnyVal {
    def mapDM[B](f: DataVector[A] => DataVector[B]): DataMatrix[B] =
      DataMatrix.is[B].coerce(mapCopyArray(m, f))

    def transpose(implicit ct: ClassTag[A]): DataMatrix[A] = {
      val nRows = m.head.length
      val newArray: Array[DataVector[A]] = new Array[DataVector[A]](nRows)

      var r = 0
      var c = 0

      while (c < nRows) {
        r = 0
        val newVec = new Array[A](m.length)
        while (r < m.length) {
          newVec(r) = m(r)(c)
          r += 1
        }
        newArray(c) = DataVector.fromArray(newVec)
        c += 1
      }
      DataMatrix.is[A].coerce(newArray)
    }
  }

}
