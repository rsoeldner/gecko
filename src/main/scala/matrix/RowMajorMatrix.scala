package matrix

import scala.reflect.ClassTag

class RowMajorMatrix[T: ClassTag](val rows: Int, val cols: Int)
    extends Matrix[T] {
  override def index(row: Int, col: Int) = row * cols + col

  def at(row: Int, col: Int) = underlaying(index(row, col))
}

object RowMajorMatrix {

  def from[T: ClassTag](values: Array[Array[T]]) = {
    val tmp = new RowMajorMatrix[T](values.length, values.head.length)
    var r = 0
    var c = 0

    while (r < tmp.rows) {
      c = 0
      while (c < tmp.cols) {
        tmp.underlaying(tmp.index(r, c)) = values(r)(c)
        c = c + 1
      }
      r = r + 1
    }

    tmp
  }

}
