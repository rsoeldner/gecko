package gecko

import scala.reflect.ClassTag

object syntax {

  implicit class ArraySyntax[T: ClassTag](val array: Array[T]) {
    def toDataVector = DataVector.fromArray(array)
  }

}
