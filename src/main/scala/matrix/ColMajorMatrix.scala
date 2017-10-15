package matrix

import scala.reflect.ClassTag

class ColMajorMatrix[T: ClassTag](val rows: Int, val cols: Int) extends Matrix[T] {
  override def index(row: Int, col: Int) = col * rows + row

  def at(row: Int, col: Int) = underlaying(index(row, col))
}

