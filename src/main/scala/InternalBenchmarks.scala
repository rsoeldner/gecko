package gecko

import java.nio.file.Paths
import cats.effect.IO
import gecko.csv.GeckoCSVUtil
import ichi.bench._
import org.saddle._
import org.saddle.io._

import scala.reflect.ClassTag

/** Benchmarks to use for benchmarking this library vs saddle
  * Otherwise, there's no reason to try to improve what we have.
  *
  */
object InternalBenchmarks {

  implicit class Sugar[A](val a: Array[A]) extends AnyVal {
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
    th.pbenchOff("Maps")(arrayGString1.map(_ + "istani").copy, ftitle = "Stdlib array map")(
      mapCopyArray[String, String](arrayGString2, _ + "istani").copy,
      htitle = "Gecko array map"
    )

    th.pbenchOff("FlatMaps")(arrayGString1.flatMap(u => Array(u + "istani")).copy, ftitle = "stdlib flatmap")(
      flatMapCopy[String, String](arrayGString2, u => Array(u + "istani")).copy,
      htitle = "Gecko flatmap"
    )

    val arrayG1: Array[Int] =
      List.range(1, 1000000).toArray
    val arrayG2: Array[Int] =
      List.range(1, 1000000).toArray
    th.pbenchOff("Maps on possibly unboxed string")(arrayG1.map(_ * 2).copy, ftitle = "Stdlib map on array")(
      mapCopyArray[Int, Int](arrayG2, _ * 2).copy,
      htitle = "Gecko map"
    )

    val defaultVector = DataVector(1, 2, 3, 4, 5)

    val largeColFrame: CDataFrame[Int, Int, Int] =
      CDataFrame.apply(DataMatrix.unsafeFromArray(Array.fill(1000)(defaultVector)))
    val largeColSaddle: Frame[Int, Int, Int] = Frame(Seq.fill(1000)(Vec(1, 2, 3, 4, 5)): _*)

    def iterateHead(frame: CDataFrame[Int, Int, Int]): Unit = {
      (1 until frame.numRows).map(frame.head).toList
      ()
    }

    def iterateHeadSaddle(frame: Frame[Int, Int, Int]): Unit = {
      (1 until frame.numRows).map(frame.head).toList
      ()
    }

    th.pbenchOff[Any]("Dataframe iteration for a long number of columns")(iterateHead(largeColFrame), ftitle = "Gecko")(
      iterateHeadSaddle(largeColSaddle),
      htitle = "Saddle"
    )
    th.pbenchOff[Any]("MapColIx for a long number of columns")(largeColFrame.mapColAt(0, _ + 30), ftitle = "Gecko")(
      largeColSaddle.mapColValues(0)(_ + 30),
      htitle = "Saddle"
    )

    val largeRowFrame =
      CDataFrame(DataMatrix.unsafeFromArray(Array.fill(5)(DataVector.fromArray(Array.range(0, 1000)))))
    val largeRowSFrame = Frame(Seq.fill(5)(Vec.arrayToVec(Array.range(0, 1000))): _*)

    /*
    NOTE: we perform worse here, but that's due to arrays requiring a copy of all references just to replace one.
     */
    th.pbenchOff[Any]("MapColIx for a long number of rows")(
      largeRowFrame.mapColAt(0, _ + 30),
      ftitle = "Gecko"
    )(
      largeRowSFrame.mapColValues(0)(_ + 30),
      htitle = "Saddle"
    )
    th.pbenchOff[Any]("IterateHead for a long number of rows")(iterateHead(largeRowFrame), ftitle = "Gecko")(
      iterateHeadSaddle(largeRowSFrame),
      htitle = "Saddle"
    )

    val fileLocation = getClass.getResource("/full_df.csv").getPath

    val readFile = fs2.io.file.readAll[IO](Paths.get(fileLocation), 4096)

    th.pbenchOff[Any]("ArrayBuffer vs saddle")(
      readFile.through(GeckoCSVUtil.csvPipe).runLast.unsafeRunSync(),
      ftitle = "ArrayBuffer"
    )(
      CsvParser.parse(CsvFile(fileLocation)),
      htitle = "Saddle"
    )
  }

}
