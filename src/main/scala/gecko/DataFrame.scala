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

  /** Return row by name
    *
    * @param r identifier
    * @return
    */
  def rowByName(r: R): Either[GeckoError, DataVector[A]] =
    rowIx.findOne(r).flatMap(rowAtIx)

  /** unsafe version, return row by name
    *
    * @param r identifier
    * @return
    */
  def unsafeRowByName(r: R): DataVector[A] =
    unsafeRowAtIx(rowIx.unsafeFindOne(r))

  /** Return column by name
    *
    * @param c identifier
    * @return
    */
  def colByName(c: C): Either[GeckoError, DataVector[A]] =
    colIx.findOne(c).flatMap(colAtIx)

  /** unsafe version, return column by name
    *
    * @param c
    * @return
    */
  def unsafeColByName(c: C): DataVector[A] = unsafeColAtIx(colIx.unsafeFindOne(c))

  /** Return DataFrame containing specified columns
    *
    * @param c identifier
    * @return
    */
  def colsByName(c: Seq[C])(implicit classTag: ClassTag[C]): Either[GeckoError, DataFrame[R, C, A]] = {
    def loop(list: List[C], acc: List[DataVector[A]]): Either[GeckoError, DataFrame[R, C, A]] =
      list match {
        case Nil if acc.isEmpty => Left(InvalidArgumentError)
        case Nil                => Right(DataFrame(rowIx, FrameIndex.fromSeq(c), DataMatrix.unsafeFromSeq(acc)))
        //          case h :: t => colIx.findOne(h).flatMap(cc => loop(t, values(cc) :: acc))
        case t => colIx.findOne(t.head).flatMap(cc => loop(t.tail, values(cc) :: acc))
      }

    loop(c.toList, List.empty[DataVector[A]])
  }

  /** Unsafe version of colsByName
    *
    * @param c
    * @param classTag
    * @return
    */
  def unsafeColsByName(c: Seq[C])(implicit classTag: ClassTag[C]): DataFrame[R, C, A] = {
    val ix: FrameIndex[C] = FrameIndex.fromSeq(c)
    val mat               = new Array[DataVector[A]](ix.length)
    var col               = 0
    while (col < ix.length) {
      mat(col) = values(colIx.unsafeFindOne(c(col)))
      col += 1
    }
    DataFrame(rowIx, ix, DataMatrix.unsafeFromArray(mat))
  }

  def dropColByIx(c: Int): Either[GeckoError, DataFrame[R, C, A]] =
    if (0 <= c && c < numCols) {
      if (numCols == 1) Left(InvalidArgumentError) //Right(DataFrame.empty[R, C, A])
      else Right(unsafeDropColByIx(c))
    } else Left(InvalidArgumentError)

  def unsafeDropColByIx(c: Int): DataFrame[R, C, A] = {
    val newArray = new Array[DataVector[A]](numRows)
    var r        = 0

    while (r < numRows) {
      newArray(r) = values(r).unsafeDrop(c)
      r += 1
    }
    DataFrame(rowIx, colIx.unsafeDropIx(c), DataMatrix.unsafeFromArray(newArray))
  }

  /** Drop column by name
    *
    * @param c identifier
    * @return
    */
  def dropColByName(c: C): Either[GeckoError, DataFrame[R, C, A]] =
    for {
      ix  <- colIx.findOne(c)
      cix <- colIx.dropIx(ix)
      v = values.drop(ix)
      mat <- DataMatrix.fromArray(v)
    } yield (DataFrame(rowIx, cix, mat))

  /** Unsafe version, drop column by name
    *
    * @param c identifier
    * @return
    */
  def unsafeDropByName(c: C): DataFrame[R, C, A] = {
    val ix  = colIx.unsafeFindOne(c)
    val cix = colIx.unsafeDropIx(ix)
    val v   = values.drop(ix)

    DataFrame[R, C, A](rowIx, cix, DataMatrix.unsafeFromArray(v))
  }

  /** Return row at specific index
    *
    * @param i index
    * @return
    */
  def rowAtIx(i: Int): Either[GeckoError, DataVector[A]] =
    if (0 <= i && i < numRows) Right(unsafeRowAtIx(i))
    else Left(RowOutOfBoundError(i, numRows))

  /** Unsafe version, return row at specific index
    *
    * @param i index
    * @return
    */
  def unsafeRowAtIx(i: Int) = values(i)

  /** Apply F over all values in a dataframe
    *
    */
  def mapValues(f: A => A): DataFrame[R, C, A] =
    DataFrame(rowIx, colIx, DataMatrix.unsafeFromArray(mapCopyArray[DataVector[A], DataVector[A]](values, _.map(f))))

  /** Transpose the frame.
    */
  def transpose: DataFrame[C, R, A] =
    DataFrame(colIx, rowIx, values.transpose)

  /** Return specific column values as DataVector[A]
    *
    * @param i index
    * @return
    */
  def colAtIx(i: Int): Either[GeckoError, DataVector[A]] =
    if (0 <= i && i < numCols) Right(unsafeColAtIx(i))
    else Left(ColOutOfBoundError(i, numCols))

  /** Unsafe version of ColAtIx
    *
    * @param i index
    * @return
    */
  def unsafeColAtIx(i: Int): DataVector[A] = {
    val newArray = new Array[A](rowIx.length)
    var j        = 0
    while (j < rowIx.length) {
      newArray(j) = values(j)(i)
      j += 1
    }
    DataVector.fromArray(newArray)
  }

  /** Apply F over a column at position i
    *
    */
  def mapColAt(i: Int, f: A => A): Either[GeckoError, DataFrame[R, C, A]] =
    if (0 <= i && i < numCols) Right(unsafeMapColAtIx(i, f))
    else Left(ColOutOfBoundError(i, numCols))

  /** Unsafe version of mapColAt
    *
    * @param i column index
    * @param f
    * @return
    */
  def unsafeMapColAtIx(i: Int, f: A => A): DataFrame[R, C, A] = {
    val newValues = copyArray(values)
    var j         = 0
    while (j < rowIx.length) {
      newValues(j) = newValues(j).unsafeReplace(i, f(newValues(j)(i)))
      j += 1
    }

    DataFrame(rowIx, colIx, DataMatrix.is[A].coerce(newValues))
  }

  /** Apply F over a row at position i
    * if i is out of bounds, it is a no-op.
    *
    */
  def mapRowAt(i: Int, f: A => A): Either[GeckoError, DataFrame[R, C, A]] =
    if (0 <= i && i < numRows) Right(unsafeMapRowAtIx(i, f))
    else Left(RowOutOfBoundError(i, numRows))

  /** Unsafe version of mapRowAt
    *
    * @param i
    * @param f
    * @return
    */
  def unsafeMapRowAtIx(i: Int, f: A => A): DataFrame[R, C, A] = {
    val newValues = copyArray(values)
    newValues(i) = newValues(i).map(f)
    DataFrame(rowIx, colIx, DataMatrix.is[A].coerce(newValues))
  }

  /** Return the first lines
    *
    * @param n number of lines
    * @return
    */
  def head(n: Int): Either[GeckoError, DataFrame[R, C, A]] =
    if (0 <= n && n < numRows) Right(unsafeHead(n))
    else Left(NotEnoughElementsError(n, numRows))

  /** Unsafe version of head
    *
    * @param n number of lines
    * @return
    */
  def unsafeHead(n: Int): DataFrame[R, C, A] = DataFrame(rowIx.unsafeSlice(0, n), colIx, values)

  /** Use specified row as column identifier/name
    *
    * @param i row index
    * @return
    */
  def withRowAsColIx(i: Int): Either[GeckoError, DataFrame[R, A, A]] = rowAtIx(i).flatMap { row =>
    val newColIx = colIx.copy(underlying = row.underlying)
    Right(DataFrame(rowIx.unsafeDropIx(i), newColIx, values))
  }

  /** Unsafe version of withRowAsColIx
    *
    * @param i row index
    * @return
    */
  def unsafeWithRowAsColIx(i: Int): DataFrame[R, A, A] = {
    val row      = unsafeRowAtIx(i)
    val newColIx = colIx.copy(underlying = row.underlying)
    DataFrame(rowIx.unsafeDropIx(i), newColIx, values)
  }

  /** Use col i's values as the row indexes
    *
    */
  def withColAsRowIx(i: Int): Either[GeckoError, DataFrame[A, C, A]] = colAtIx(i).flatMap { col =>
    val newRowIx = rowIx.copy(underlying = col.underlying)
    Right(DataFrame(newRowIx, colIx.unsafeDropIx(i), values))
  }

  /** Unsafe version of withColAsRowIx
    *
    * @param i col index
    * @return
    */
  def unsafeWithColAsRowIx(i: Int): DataFrame[A, C, A] = {
    val col      = unsafeColAtIx(i)
    val newRowIx = rowIx.copy(underlying = col.underlying)
    DataFrame(newRowIx, colIx.unsafeDropIx(i), values)
  }

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
      var r    = 0

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
          listMap                         <- list
          elem: (A, DataFrame[Int, C, A]) <- listMap._2.groupBy_(dimension).right.get
        } yield (listMap._1.updated(dimension, elem._1), elem._2)
    }
  }

  /**
    * Shorthand unsafeConcat, row-wise
    * @param other
    * @return
    */
  def ++(other: DataFrame[R, C, A]): DataFrame[R, C, A] = unsafeConcat(other)

  /**
    * unsafeConcat row-wise
    * @param other
    * @return
    */
  def unsafeConcat(other: DataFrame[R, C, A]): DataFrame[R, C, A] = {
    val newRowIx  = rowIx ++ other.rowIx
    val newValues = Array.concat(values, other.values)
    DataFrame(newRowIx, colIx, DataMatrix.unsafeFromArray(newValues))
  }

  /**
    * concat row-wise
    * @param other
    * @return
    */
  def concat(other: DataFrame[R, C, A]): Either[GeckoError, DataFrame[R, C, A]] =
    if (this.numCols == other.numCols && colIx == other.colIx) Right(unsafeConcat(other))
    else Left(InvalidArgumentError)

  /** Replace row by v
    *
    * @param row index
    * @param v new values
    * @return
    */
  def replaceRow(row: Int, v: DataVector[A]): Either[GeckoError, DataFrame[R, C, A]] =
    if (0 <= row && row < numRows && v.length == numCols) Right(unsafeReplaceRow(row, v))
    else Left(InvalidArgumentError)

  /** Unsafe version, replace row by v
    *
    * @param row index
    * @param v new values
    * @return
    */
  def unsafeReplaceRow(row: Int, v: DataVector[A]): DataFrame[R, C, A] = {
    val newArr: Array[DataVector[A]] = values.clone()
    newArr(row) = v
    DataFrame(rowIx, colIx, DataMatrix.unsafeFromArray(newArr))
  }

  /** Replace column by v
    *
    * @param col index
    * @param v new values
    * @return
    */
  def replaceCol(col: Int, v: DataVector[A]): Either[GeckoError, DataFrame[R, C, A]] =
    if (0 <= col && col < numCols && v.length == numRows) Right(unsafeReplaceCol(col, v))
    else Left(InvalidArgumentError)

  /** Unsafe version, replace column by v
    *
    * @param col index
    * @param v new values
    * @return
    */
  def unsafeReplaceCol(col: Int, v: DataVector[A]): DataFrame[R, C, A] = {
    val newArr: Array[DataVector[A]] = values.clone()
    var i                            = 0
    while (i < numRows) {
      newArr(i).unsafeReplace(col, v(i))
      i += 1
    }
    DataFrame(rowIx, colIx, DataMatrix.unsafeFromArray(newArr))
  }

}

object DataFrame {

  def apply[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag : EmptyPrint](
      rowIx: FrameIndex[R],
      colIx: FrameIndex[C],
      values: DataMatrix[A]
  ): DataFrame[R, C, A] = new DataFrame[R, C, A](values, rowIx, colIx) {}

  def default[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyPrint](
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

  def fill[A: ClassTag: EmptyGecko: EmptyPrint](n: Int, vector: DataVector[A]): DataFrame[Int, Int, A] =
    default(DataMatrix.fill(n, vector))

}
