package frame

import matrix.{Matrix, RowMajorMatrix}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

class Frame[T: ClassTag, R: ClassTag, C: ClassTag](rows: Int, cols: Int) {

  lazy val colIdx: Index[C] = new Index[C](cols)
  lazy val rowIdx: Index[R] = new Index[R](rows)

  lazy val raw: Matrix[T] = new RowMajorMatrix[T](rows, cols)

  val colMapping: Mapping = new Mapping

  def underlaying = raw

  override def toString: String = {
    val string = new StringBuilder
    var r = 0
    var c = 0

    while (c < cols) {
      string.append(',')
      string.append(colIdx.at(c))
      c = c + 1
    }

    while (r < rows) {
      c = 0
      string.append('\n')
      string.append(rowIdx.at(r))
      while (c < cols) {
        string.append(',')
        string.append(raw.at(r, c))
        c = c+1
      }
      r = r+1
    }
    string.toString()
  }

}

object Frame {

  def checkIfNumeric(values: Array[Array[String]], rows: Int, cols: Int) = {
    val isNumeric: Array[Boolean] = Array.fill(cols)(true)

    var c = 0
    var r = 0
    while (c < cols) {
      r = 0
      while (r < rows && isNumeric(c)) {
        try {
          values(r)(c).toDouble
        } catch {
          case _: Exception => isNumeric(c) = false
        }
        r = r + 1
      }
      c = c + 1
    }
    isNumeric
  }

  def from(values: Array[Array[String]]) = {
    val rows = values.length
    val cols = values.head.length

    val frame = new Frame[Float, String, String](rows, cols)

    val isNumericColumn = checkIfNumeric(values, rows, cols)

    var r = 0
    var c = 0

    val rowIndex = new ListBuffer[String]()
    while (r < rows) {
      rowIndex += values(r)(0)
      r = r + 1
    }

    frame.colIdx.from(values.head)
    while (c < cols) {
      r = 1
      while (r < rows) {
        if (!isNumericColumn(c)) {
          val v = values(r)(c)
          val t = frame.colMapping.getOrUpdate(v)
          frame.raw.underlaying(frame.raw.index(r, c)) = t
        } else
          frame.raw.underlaying(frame.raw.index(r, c)) = values(r)(c).toFloat

        r = r + 1
      }
      c = c + 1
    }
    frame.rowIdx.from(rowIndex.toArray)
    frame
  }

}
