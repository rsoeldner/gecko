package tentative
import ichi.bench._

import scala.reflect.ClassTag

/** Benchmarks to use for benchmarking this library vs saddle
  * Otherwise, there's no reason to try to improve what we have.
  *
  */
object InternalBenchmarks {

  implicit class kek[A](val a: Array[A]) extends AnyVal {
    def copy(implicit c: ClassTag[A]): Array[A] = {
      val arr = new Array[A](a.length)
      System.arraycopy(a, 0, arr, 0, a.length)
      arr
    }

  }

  def main(args: Array[String]): Unit = {

    val th = Thyme.warmed(verbose = print)

    val arrayGString1: Array[String] =
      List.range(1, 1000000).map("kekistani" + _).toArray
    val arrayGString2: Array[String] =
      List.range(1, 1000000).map("kekistani" + _).toArray
    th.pbenchOff("Maps")(arrayGString1.map(_ + "istani").copy)(
      mapCopyArray[String, String](arrayGString2, _ + "istani").copy
    )

    th.pbenchOff("FlatMaps")(arrayGString1.flatMap(u => Array(u + "istani")).copy)(
      flatMapCopy[String, String](arrayGString2, u => Array(u + "istani")).copy
    )

    val arrayG1: Array[Int] =
      List.range(1, 1000000).toArray
    val arrayG2: Array[Int] =
      List.range(1, 1000000).toArray
    th.pbenchOff("Maps")(arrayG1.map(_ * 2).copy)(mapCopyArray[Int, Int](arrayG2, _ * 2).copy)


    val ayy = DataVector(1, 2, 3, 4, 5)

    println(ayy ++ DataVector.empty[Int])
  }

}
