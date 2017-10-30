package gecko

import scala.reflect.ClassTag

/** A colwise dataframe object.
  * Useful for transformations involving
  *
  */
sealed abstract class CDataFrame[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag](
    private[gecko] val values: DataMatrix[A],
    rowIx: FrameIndex[R],
    colIx: FrameIndex[C]
)(implicit emptyGecko: EmptyGecko[A]) {

  /** Return the number of rows
    *
    */
  def numRows: Int = rowIx.length

  def numCols: Int = colIx.length

  def rowAtIx(i: Int): DataVector[A] =
    if (i >= rowIx.length || i < 0)
      DataVector.empty[A]
    else {
      val newArray = new Array[A](colIx.length)
      var j        = 0
      val rowIndex = rowIx.index(i)
      while (j < colIx.length) {
        newArray(j) = values(colIx.index(j))(rowIndex)
        j += 1
      }
      DataVector.fromArray(newArray)
    }

  def colAtIx(i: Int): DataVector[A] =
    if (i >= rowIx.length || i < 0)
      DataVector.empty[A]
    else {
      values(colIx.index(i))
    }

  def mapColAt(i: Int, f: A => A): CDataFrame[R, C, A]

  def mapRowAt(i: Int, f: A => A): CDataFrame[R, C, A]

  def transpose: DataFrame[C, R, A]

  def head(n: Int): CDataFrame[R, C, A]

  /** Shitty to string
    *
    */
  override def toString: String = {
    val builder = new java.lang.StringBuilder()
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
}

object CDataFrame {
  def default[@specialized(Int, Double, Boolean, Long) A: ClassTag](arr: DataMatrix[A]): CDataFrame[Int, Int, A] =
    if (arr.isEmpty) {
      empty[Int, Int, A]
    } else {
      val rows = FrameIndex.default(arr.head.length)
      val cols = FrameIndex.default(arr.length)
      apply[Int, Int, A](rows, cols, arr)
    }

  def empty[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag]: CDataFrame[R, C, A] =
    apply(FrameIndex.empty[R], FrameIndex.empty[C], DataMatrix.empty[A])

  def apply[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      arr: DataMatrix[A]
  ): CDataFrame[Int, Int, A] =
    if (arr.isEmpty)
      empty[Int, Int, A]
    else {
      val rows = FrameIndex.default(arr(0).length)
      val cols = FrameIndex.default(arr.length)
      apply[Int, Int, A](rows, cols, arr)
    }

  def apply[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag](
      rowIx: FrameIndex[R],
      colIx: FrameIndex[C],
      values: DataMatrix[A]
  ): CDataFrame[R, C, A] = new CDataFrame[R, C, A](values, rowIx, colIx) {
    def head(n: Int): CDataFrame[R, C, A] =
      apply(rowIx.slice(0, n), colIx, values)

    def transpose: DataFrame[C, R, A] = DataFrame(colIx, rowIx, values)

    def mapColAt(i: Int, f: (A) => A): CDataFrame[R, C, A] =
      if (i >= rowIx.length || i < 0)
        this
      else {
        val newValues = copyArray(values)
        newValues(colIx.index(i)) = newValues(colIx.index(i)).map(f)
        apply(rowIx, colIx, DataMatrix.is[A].coerce(newValues))
      }

    def mapRowAt(i: Int, f: (A) => A): CDataFrame[R, C, A] =
      if (i >= rowIx.length || i < 0)
        this
      else {
        val newArray = copyArray(values)
        val rowIndex = rowIx.index(i)
        var j        = 0
        while (j < numCols) {
          newArray(j) = newArray(j).replace(rowIndex, f(newArray(j)(rowIndex)))
          j += 1
        }

        apply(rowIx, colIx, DataMatrix.unsafeFromArray(newArray))
      }
  }

  def fill[A: ClassTag: EmptyGecko](n: Int, vector: DataVector[A]): CDataFrame[Int, Int, A] =
    default(DataMatrix.fill(n, vector))

}
