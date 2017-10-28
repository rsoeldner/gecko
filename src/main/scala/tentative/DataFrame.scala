package tentative

import scala.reflect.ClassTag

sealed abstract case class DataFrame[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag](
    private[DataFrame] val values: Array[DataVector[A]],
    rowIx: FrameIndex[R],
    colIx: FrameIndex[C]
) {
    def rowAtIx(i: Int): Array[A] = ???

}
