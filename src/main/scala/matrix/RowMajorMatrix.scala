package matrix

import scala.reflect.ClassTag

class RowMajorMatrix[T: ClassTag](val rows: Int, val cols: Int) extends Matrix[T] {
  override def index(row: Int, col: Int) = row * cols + col

  override def apply(row: Int, col: Int) = underlaying(index(row, col))
}
