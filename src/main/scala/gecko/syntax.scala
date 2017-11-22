package gecko

import scala.reflect.ClassTag

object syntax {

  implicit class ArraySyntax[T: ClassTag: EmptyGecko](val array: Array[T]) {
    def toDataVector: DataVector[T] = DataVector.fromArray(array)
  }

  implicit class FrameIndexSyntax[T: ClassTag](elem: Seq[T]) {
    implicit def toIndex: FrameIndex[T] = FrameIndex(elem.toArray)
  }

}
