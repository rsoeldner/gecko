package gecko.csv

import java.nio.charset.StandardCharsets
import cats.effect.Sync
import gecko._
import fs2.Stream

import scala.reflect.ClassTag

object GeckoCSVUtil {

  def streamFrame[F[_]: Sync, R, C, A: ClassTag](
      frame: DataFrame[R, C, A]
  )(implicit g: EmptyGecko[A]): Stream[F, Byte] = {
    def colIxBytes(vec: FrameIndex[C]): Array[Byte] = {
      val stringBuff = new java.lang.StringBuilder()
      var i          = 0
      while (i < vec.length - 1) {
        stringBuff.append(vec(i))
        stringBuff.append(",")
        i += 1
      }
      stringBuff.append(vec(i))
      stringBuff.toString.getBytes(StandardCharsets.US_ASCII)
    }

    def rowBytes(vec: DataVector[A]): Array[Byte] = {
      val stringBuff = new java.lang.StringBuilder()
      var i          = 0
      stringBuff.append("\n")
      while (i < vec.length - 1) {
        stringBuff.append(vec.at(i).getOrElse("nan"))
        stringBuff.append(",")
        i += 1
      }
      stringBuff.append(vec.at(i).getOrElse("nan"))
      stringBuff.toString.getBytes(StandardCharsets.US_ASCII)
    }

    Stream.emits(colIxBytes(frame.colIx)).covary[F] ++
      Stream
        .emits(Array.range(0, frame.numRows))
        .flatMap(i => Stream.emits(rowBytes(frame.rowAtIx(i))))
        .covary[F]
  }
}
