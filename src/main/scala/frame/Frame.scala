package frame

import matrix.Matrix

trait Frame[T, R, C] {

  val colIdx: Index[C]
  val rowIdx: Index[R]

  val raw: Matrix[T]

  def underlaying = raw

}
