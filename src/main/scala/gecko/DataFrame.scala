package gecko

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

/** A row-stored dataframe object.
  * Useful for transformations involving rows over just columns
  *
  */
sealed abstract class DataFrame[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag](
    private[gecko] val values: DataMatrix[A],
    val rowIx: FrameIndex[R],
    val colIx: FrameIndex[C]
)(implicit emptyGecko: EmptyGecko[A],
  emptyPrint: EmptyPrint[A]) {

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
  def row(r: R): DataVector[A]

  /** return selected column
    *
    * @param c
    * @return
    */
  def col(c: C): DataVector[A]

  /** Return the row at a specified index
    * If the index if out of bounds, it will return an empty vector
    *
    */
  def rowAtIx(i: Int): DataVector[A] =
    if (i >= rowIx.length || i < 0)
      DataVector.empty[A]
    else {
      values(rowIx.index(i))
    }

  /** Apply F over all values in a dataframe
    *
    */
  def mapValues(f: A => A): DataFrame[R, C, A]

  /** Transpose the frame.
    * Given CDataframe is column-represented, it works perfectly for this purpose
    *
    */
  def transpose: CDataFrame[C, R, A]

  /** Retrieve the column at position i, as a DataVector
    *
    */
  def colAtIx(i: Int): DataVector[A] =
    if (i >= colIx.length || i < 0)
      DataVector.empty[A]
    else {
      val newArray = new Array[A](rowIx.length)
      var j        = 0
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
  def mapColAt(i: Int, f: A => A): DataFrame[R, C, A]

  /** Apply F over a row at position i
    * if i is out of bounds, it is a no-op.
    *
    */
  def mapRowAt(i: Int, f: A => A): DataFrame[R, C, A]

  /** Return the first n elements
    *
    */
  def head(n: Int): DataFrame[R, C, A]

  /** Use row i's values as the column index labels/values
    *
    */
  def withRowAsColIx(i: Int): DataFrame[R, A, A]

  /** Use col i's values as the row indexes
    *
    */
  def withColAsRowIx(i: Int): DataFrame[A, C, A]

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

  def groupBy_(c: C): List[(A, DataFrame[Int, C, A])] = {
    val book = new mutable.ListMap[A, mutable.ListBuffer[DataVector[A]]]()
    var r = 0
    val idx = colIx.findOne(c)

    while(r < numRows) {
      val entry =values(r)(idx)
      book.getOrElseUpdate(entry, new ListBuffer[DataVector[A]]) += rowAtIx(r)

      r += 1
    }
    book.mapValues{buf =>
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
      case (list, dimension) => for {
        listMap <- list
        elem: (A, DataFrame[Int, C, A]) <- listMap._2.groupBy_(dimension)
      } yield (listMap._1.updated(dimension, elem._1), elem._2)
    }
  }

  /** Concat two DataFrame's row-wise
    *
    * @param other
    * @return
    */
  def ++(other: DataFrame[R,C,A]): DataFrame[R, C, A] = {
    val newRowIx = rowIx ++ other.rowIx
    val newValues = Array.concat(values, other.values)
    DataFrame(newRowIx,colIx, DataMatrix.unsafeFromArray(newValues))
  }

}

object DataFrame {
  def default[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyPrint](arr: DataMatrix[A]): DataFrame[Int, Int, A] =
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
    def head(n: Int): DataFrame[R, C, A] =
      apply(rowIx.slice(0, n), colIx, values)

    def mapValues(f: (A) => A): DataFrame[R, C, A] =
      apply(rowIx, colIx, DataMatrix.unsafeFromArray(mapCopyArray[DataVector[A], DataVector[A]](values, _.map(f))))

    def transpose: CDataFrame[C, R, A] = CDataFrame(colIx, rowIx, values)

    def mapColAt(i: Int, f: (A) => A): DataFrame[R, C, A] =
      if (i >= colIx.length || i < 0)
        this
      else {
        val newValues = copyArray(values)
        var j         = 0
        val colIndex  = colIx.index(i)
        while (j < rowIx.length) {
          newValues(j) = newValues(j).replace(colIndex, f(newValues(j)(colIndex)))
          j += 1
        }

        apply(rowIx, colIx, DataMatrix.is[A].coerce(newValues))
      }

    def mapRowAt(i: Int, f: (A) => A): DataFrame[R, C, A] =
      if (i >= rowIx.length || i < 0)
        this
      else {
        val newValues = copyArray(values)
        newValues(rowIx.index(i)) = newValues(rowIx.index(i)).map(f)
        apply(rowIx, colIx, DataMatrix.is[A].coerce(newValues))
      }

    def row(r: R): DataVector[A] = rowAtIx(rowIx.findOne(r))

    def col(c: C): DataVector[A] = colAtIx(colIx.findOne(c))

    def withRowAsColIx(i: Int): DataFrame[R, A, A] = {
      val newColIx = colIx.copy(underlying = rowAtIx(i).underlying)
      apply(rowIx.removeIx(i), newColIx, values)
    }

    def withColAsRowIx(i: Int): DataFrame[A, C, A] = {
      val newRowIx = rowIx.copy(underlying = colAtIx(i).underlying)
      apply(newRowIx, colIx.removeIx(i), values)
    }
  }

  def fill[A: ClassTag: EmptyGecko: EmptyPrint](n: Int, vector: DataVector[A]): DataFrame[Int, Int, A] =
    default(DataMatrix.fill(n, vector))

}
