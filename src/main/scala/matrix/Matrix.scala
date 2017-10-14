package matrix

import scala.reflect.ClassTag

abstract class Matrix[T: ClassTag] {

  val rows: Int
  val cols: Int

  val length = rows * cols


  private val raw = new Array[T](length)

  def underlaying = raw

  def fillWith(default: T): Unit = {
    var n = 0
    while (n < length) {
      raw(n) = default
      n = n + 1
    }
  }

  def index(row: Int, col: Int): Int
  def apply(row: Int, col: Int): T
  

  def toRawString: String = underlaying.mkString("Raw[", ",", "]")

  override def toString: String = {
    val string = new StringBuilder
    var r=0
    var c=0

    while(r < rows) {
      c = 0
      if(r > 0) string.append('\n')

      while(c < cols) {
        if(c > 0) string.append(", ")
        string.append(apply(r,c))
        c = c+1
      }
      r = r+1
    }
  string.toString()
  }

}