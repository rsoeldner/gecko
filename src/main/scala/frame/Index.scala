package frame

import scala.reflect.ClassTag

class Index[T: ClassTag](size: Int) {

  private val raw = new Array[T](size)

  def from(elem: Array[T]): Either[Throwable, Unit] = {
    if(elem.size != size) return Left(new ArrayIndexOutOfBoundsException)

    var n = 0
    while(n < size) {
      raw(n) = elem(n)
      n = n+1
    }
    Right(())
  }

  def contains(key: T): Option[Int] = {
    var n = 0
    while(n < size) {
      if(raw(n) == key) return Some(n)

      n = n+1
    }
    None
  }
}
