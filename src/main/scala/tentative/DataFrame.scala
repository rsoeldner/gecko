package tentative

import scala.reflect.ClassTag

sealed abstract class DataFrame[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag](
    private[tentative] val values: DataMatrix[A],
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
      while (j < colIx.length) {
        newArray(j) = values(j)(i)
        j += 1
      }
      DataVector.fromArray(newArray)
    }

  def colAtIx(i: Int): DataVector[A] =
    if (i >= rowIx.length || i < 0)
      DataVector.empty[A]
    else {
      values(i)
    }

  def mapColIx(i: Int, f: A => A): DataFrame[R, C, A]

  def head(n: Int): DataFrame[R, C, A]

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
}

object DataFrame {
  def apply[@specialized(Int, Double, Boolean, Long) A: ClassTag](arr: DataMatrix[A]): DataFrame[Int, Int, A] =
    if (arr.isEmpty) {
      empty[Int, Int, A]
    } else {
      val rows = FrameIndex.default(arr.head.underlying.length) //todo: Fix this ugly shit
      val cols = FrameIndex.default(arr.length)
      build[Int, Int, A](rows, cols, arr)
    }

  def empty[R, C, A]: DataFrame[R, C, A] = ???

  def fromArray[@specialized(Int, Double, Boolean, Long) A: ClassTag](
      arr: DataMatrix[A]
  ): DataFrame[Int, Int, A] =
    if (arr.isEmpty)
      empty[Int, Int, A]
    else {
      val rows = FrameIndex.default(arr(0).length)
      val cols = FrameIndex.default(arr.length)
      build[Int, Int, A](rows, cols, arr)
    }

  private final def build[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag](
      rowIx: FrameIndex[R],
      colIx: FrameIndex[C],
      values: DataMatrix[A]
  ): DataFrame[R, C, A] = new DataFrame[R, C, A](values, rowIx, colIx) {
    def head(n: Int): DataFrame[R, C, A] =
      build(rowIx.slice(0, n), colIx, values)

    def mapColIx(i: Int, f: (A) => A): DataFrame[R, C, A] =
      if (i >= rowIx.length || i < 0)
        this
      else {
        val newValues = copyArray(values)
        newValues(i) = newValues(i).map(f)
        build(rowIx, colIx, DataMatrix.is[A].coerce(newValues))
      }
  }

}
