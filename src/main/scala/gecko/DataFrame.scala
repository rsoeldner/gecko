package gecko

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

import cats.implicits._
import cats.data._

/** A row-stored dataframe object.
  * Useful for transformations involving rows over just columns
  *
  */
sealed abstract class DataFrame[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag](
                                                                                             private[gecko] val values: DataMatrix[A],
                                                                                             val rowIx: FrameIndex[R],
                                                                                             val colIx: FrameIndex[C]
                                                                                           )(implicit emptyGecko: EmptyGecko[A], emptyPrint: EmptyPrint[A]) {

  /** Number of Rows
    *
    * @return
    */
  def numRows: Int = rowIx.length

  /** Number of Columns
    *
    * @return
    */
  def numCols: Int = colIx.length

  /** Return selected row
    *
    * @param r index
    * @return
    */
  def row(r: R): Either[GeckoError, DataVector[A]]

  def unsafeRow(r: R): DataVector[A]

  /** return selected column
    *
    * @param c
    * @return
    */
  def col(c: C): Either[GeckoError, DataVector[A]]

  def unsafeCol(c: C): DataVector[A]

  /** Return specified columns
    *
    * @param c
    * @return
    */
  def cols(c: Seq[C])(implicit classTag: ClassTag[C]): Either[GeckoError, DataFrame[R, C, A]]

  def unsafeCols(c: C*): DataFrame[R, C, A]

  def dropCol(c: C): Either[GeckoError, DataFrame[R, C, A]]

  def unsafeDropCol(c: C): DataFrame[R, C, A]

  def rowAtIx(i: Int): Either[GeckoError, DataVector[A]] =
    if (0 <= i && i < numRows) Right(unsafeColAtIx(i))
    else Left(RowOutOfBoundError(i, numRows))

  def unsafeRowAtIx(i: Int) = values(rowIx.index(i))

  /** Apply F over all values in a dataframe
    *
    */
  def mapValues(f: A => A): DataFrame[R, C, A]

  /** Transpose the frame.
    * Given CDataframe is column-represented, it works perfectly for this purpose
    *
    */
  def transpose: DataFrame[C, R, A]

  /** Retrieve the column at position i, as a DataVector
    *
    */
  def colAtIx(i: Int): Either[GeckoError, DataVector[A]] =
    if (0 <= i && i < numCols) Right(unsafeColAtIx(i))
    else Left(ColOutOfBoundError(i, numCols))

  def unsafeColAtIx(i: Int): DataVector[A] = {
    val newArray = new Array[A](rowIx.length)
    var j = 0
    val colIndex = colIx.index(i)
    while (j < rowIx.length) {
      newArray(j) = values(rowIx.index(i))(colIndex)
      j += 1
    }
    DataVector.fromArray(newArray)
  }

  /** Apply F over a column at position i
    *
    */
  def mapColAt(i: Int, f: A => A): Either[GeckoError, DataFrame[R, C, A]]

  def unsafeMapColAt(i: Int, f: A => A): DataFrame[R, C, A]

  /** Apply F over a row at position i
    * if i is out of bounds, it is a no-op.
    *
    */
  def mapRowAt(i: Int, f: A => A): Either[GeckoError, DataFrame[R, C, A]]

  def unsafeMapRowAt(i: Int, f: A => A): DataFrame[R, C, A]

  /** Return the first n elements
    *
    */
  def head(n: Int): Either[GeckoError, DataFrame[R, C, A]]

  def unsafeHead(n: Int): DataFrame[R, C, A]

  /** Use row i's values as the column index labels/values
    *
    */
  def withRowAsColIx(i: Int): Either[GeckoError, DataFrame[R, A, A]]

  def unsafeWithRowAsColIx(i: Int): DataFrame[R, A, A]

  /** Use col i's values as the row indexes
    *
    */
  def withColAsRowIx(i: Int): Either[GeckoError, DataFrame[A, C, A]]

  def unsafeWithColAsRowIx(i: Int): DataFrame[A, C, A]

  /** Shitty to string
    *
    */
  override def toString: String = {
    val builder = new java.lang.StringBuilder()
    builder.append(s"[$numRows x $numCols]\n")
    builder.append(s"Columns: ${colIx.underlying.mkString(" ")}\n")
    var i = 0
    if (numRows < 6) {
      while (i < rowIx.length) {
        builder.append(s"${rowIx.underlying(i)} ${rowAtIx(i)}\n")
        builder.capacity()
        i += 1
      }
    } else {
      builder.append(s"${rowIx.underlying(0)} ${rowAtIx(0)}\n")
      builder.append(s"${rowIx.underlying(1)} ${rowAtIx(1)}\n")
      builder.append("...\n")
      builder.append(s"${rowIx.underlying(numRows - 2)} ${rowAtIx(numRows - 2)}\n")
      builder.append(s"${rowIx.underlying(numRows - 1)} ${rowAtIx(numRows - 1)}\n")
    }

    builder.toString
  }

  def printPretty: Unit = {
    println(colIx.underlying.mkString(" "))
    var i = 0
    while (i < numRows) {
      println(s"${rowIx(i)}: ${rowAtIx(i)}")
      i += 1
    }
  }

  /** GroupBy single column
    *
    * @param c Column name
    * @return
    */
  def groupBy_(c: C): Either[GeckoError, List[(A, DataFrame[Int, C, A])]] =
    colIx.findOne(c).map { idx =>
      val book = new mutable.ListMap[A, mutable.ListBuffer[DataVector[A]]]()
      var r = 0

      while (r < numRows) {
        val entry = values(r)(idx)
        book.getOrElseUpdate(entry, new ListBuffer[DataVector[A]]) += unsafeRowAtIx(r)

        r += 1
      }
      book.mapValues { buf =>
        val matrix = DataMatrix.unsafeFromArray(buf.result().toArray)
        DataFrame(FrameIndex.default(matrix.length), colIx, matrix)
      }.toList
    }

  /** GroupBy list of dimensions
    *
    * @param dim List of dimensions from left to right
    * @return
    */
  def groupBy(dim: List[C]): List[(Map[C, A], DataFrame[Int, C, A])] = {
    val thisFrame = DataFrame(FrameIndex.default(numRows), colIx, values)
    dim.foldLeft(List((Map.empty[C, A], thisFrame))) {
      case (list, dimension) =>
        for {
          listMap <- list
          elem: (A, DataFrame[Int, C, A]) <- listMap._2.groupBy_(dimension).right.get
        } yield (listMap._1.updated(dimension, elem._1), elem._2)
    }
  }

  /** Concat two DataFrame's row-wise
    *
    * @param other
    * @return
    */
  def ++(other: DataFrame[R, C, A]): DataFrame[R, C, A] = {
    val newRowIx = rowIx ++ other.rowIx
    val newValues = Array.concat(values, other.values)
    DataFrame(newRowIx, colIx, DataMatrix.unsafeFromArray(newValues))
  }

}

object DataFrame {
  def default[@specialized(Int, Double, Boolean, Long) A: ClassTag : EmptyPrint](
                                                                                  arr: DataMatrix[A]
                                                                                ): DataFrame[Int, Int, A] =
    if (arr.isEmpty) {
      empty[Int, Int, A]
    } else {
      val rows = FrameIndex.default(arr.length) //todo: Fix this ugly shit
      val cols = FrameIndex.default(arr.head.length)
      apply[Int, Int, A](rows, cols, arr)
    }

  def empty[R: ClassTag, C: ClassTag, @specialized(Int, Double, Boolean, Long) A: ClassTag]: DataFrame[R, C, A] =
    DataFrame(FrameIndex.empty[R], FrameIndex.empty[C], DataMatrix.empty[A])

  def apply[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag](
                                                                         rowIx: FrameIndex[R],
                                                                         colIx: FrameIndex[C],
                                                                         values: DataMatrix[A]
                                                                       )(implicit emptyPrint: EmptyPrint[A]): DataFrame[R, C, A] = new DataFrame[R, C, A](values, rowIx, colIx) {

    def unsafeHead(n: Int): DataFrame[R, C, A] = apply(rowIx.unsafeSlice(0, n), colIx, values)

    def head(n: Int): Either[GeckoError, DataFrame[R, C, A]] =
      if (0 <= n && n < numRows) Right(unsafeHead(n))
      else Left(NotEnoughElementsError(n, numRows))

    def mapValues(f: (A) => A): DataFrame[R, C, A] =
      apply(rowIx, colIx, DataMatrix.unsafeFromArray(mapCopyArray[DataVector[A], DataVector[A]](values, _.map(f))))

    def transpose: DataFrame[C, R, A] = DataFrame[C, R, A](colIx, rowIx, values.transpose)

    def unsafeMapColAt(i: Int, f: (A) => A): DataFrame[R, C, A] = {
      val newValues = copyArray(values)
      var j = 0
      val colIndex = colIx.index(i)
      while (j < rowIx.length) {
        newValues(j) = newValues(j).unsafeReplace(colIndex, f(newValues(j)(colIndex)))
        j += 1
      }

      apply(rowIx, colIx, DataMatrix.is[A].coerce(newValues))
    }

    def mapColAt(i: Int, f: A => A): Either[GeckoError, DataFrame[R, C, A]] =
      if (0 <= i && i < numCols) Right(unsafeMapColAt(i, f))
      else Left(ColOutOfBoundError(i, numCols))

    def unsafeMapRowAt(i: Int, f: (A) => A): DataFrame[R, C, A] = {
      val newValues = copyArray(values)
      newValues(rowIx.index(i)) = newValues(rowIx.index(i)).map(f)
      apply(rowIx, colIx, DataMatrix.is[A].coerce(newValues))
    }

    def mapRowAt(i: Int, f: A => A): Either[GeckoError, DataFrame[R, C, A]] =
      if (0 <= i && i < numRows) Right(unsafeMapRowAt(i, f))
      else Left(RowOutOfBoundError(i, numRows))

    def row(r: R): Either[GeckoError, DataVector[A]] = rowIx.findOne(r).flatMap(rowAtIx)

    def unsafeRow(r: R): DataVector[A] = unsafeRowAtIx(rowIx.unsafeFindOne(r))

    def col(c: C): Either[GeckoError, DataVector[A]] = colIx.findOne(c).flatMap(colAtIx)

    def unsafeCol(c: C): DataVector[A] = unsafeColAtIx(colIx.unsafeFindOne(c))

    def withRowAsColIx(i: Int): Either[GeckoError, DataFrame[R, A, A]] = rowAtIx(i).flatMap { row =>
      val newColIx = colIx.copy(underlying = row.underlying)
      Right(apply(rowIx.unsafeRemoveIx(i), newColIx, values))
    }

    def unsafeWithRowAsColIx(i: Int): DataFrame[R, A, A] = {
      val row = unsafeRowAtIx(i)
      val newColIx = colIx.copy(underlying = row.underlying)
      apply(rowIx.unsafeRemoveIx(i), newColIx, values)
    }

    def withColAsRowIx(i: Int): Either[GeckoError, DataFrame[A, C, A]] = colAtIx(i).flatMap { col =>
      val newRowIx = rowIx.copy(underlying = col.underlying)
      Right(apply(newRowIx, colIx.unsafeRemoveIx(i), values))
    }

    def unsafeWithColAsRowIx(i: Int): DataFrame[A, C, A] = {
      val col = unsafeColAtIx(i)
      val newRowIx = rowIx.copy(underlying = col.underlying)
      apply(newRowIx, colIx.unsafeRemoveIx(i), values)
    }

    /** Return specified columns
      *
      * @param c
      * @return
      */
    def cols(c: Seq[C])(implicit classTag: ClassTag[C]): Either[GeckoError, DataFrame[R, C, A]] = {
      def loop(list: List[C], acc: List[DataVector[A]]): Either[GeckoError, DataFrame[R, C, A]] = {
        list match {
          case Nil if acc.isEmpty=> Left(InvalidArgumentError)
          case Nil => Right(DataFrame(rowIx, FrameIndex.fromSeq(c), DataMatrix.unsafeFromSeq(acc)))
//          case h :: t => colIx.findOne(h).flatMap(cc => loop(t, values(cc) :: acc))
          case t => colIx.findOne(t.head).flatMap(cc => loop(t.tail, values(cc) :: acc))
        }
      }

      loop(c.toList, List.empty[DataVector[A]])
    }

    def unsafeCols(c: C*) = ???

    def dropCol(c: C) = for {
      ix <- colIx.findOne(c)
      cix <- colIx.removeIx(ix)
      v = values.drop(ix)
      mat <- DataMatrix.fromArray(v)
    } yield (DataFrame(rowIx, cix, mat))

    def unsafeDropCol(c: C) = {
      val ix = colIx.unsafeFindOne(c)
      val cix = colIx.unsafeRemoveIx(ix)
      val v = values.drop(ix)

      DataFrame[R, C, A](rowIx, cix, DataMatrix.unsafeFromArray(v))
    }

    def fill[A: ClassTag : EmptyGecko : EmptyPrint](n: Int, vector: DataVector[A]): DataFrame[Int, Int, A] =
      default(DataMatrix.fill(n, vector))

  }
}
