package gecko.csv

import java.nio.charset.StandardCharsets

import cats.effect.{Effect, Sync}
import gecko._
import fs2._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag

object GeckoCSVUtil {

  private val quoteChar = '"'
  private val separator = ','

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

  def csvPipe[F[_]](implicit F: Effect[F]): Pipe[F, Byte, DataFrame[Int, Int, String]] = { s =>
    val stream = s
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(!_.isEmpty)

    Stream.eval(F.map(stream.head.flatMap { str =>
      val csvArray = DataVector.fromArray(parseSimpleCSV(str))
      Stream.emit(csvArray) ++ stream.tail.evalMap(parseEffectLen[F](_, csvArray.length))
    }.runLog)(v => DataFrame.default(DataMatrix.unsafeFromArray(v.toArray))))
  }

  def parseEffectLen[F[_]](line: String, lineLen: Int, stripQuote: Boolean = true)(
      implicit F: Effect[F]
  ): F[DataVector[String]] = {
    val parsed = DataVector.fromArray(parseSimpleCSV(line, stripQuote))
    if (parsed.length != lineLen)
      F.raiseError[DataVector[String]](InvalidCSVError("Vectors do not have an even number of columns"))
    else
      F.pure(parsed)
  }

  def parseSimpleCSV(line: String, stripQuote: Boolean = true): Array[String] = {
    val result = ArrayBuffer[String]()

    var inQ = false // whether our scan is between quotes

    var curFld = 0 // current field of parse
    var curBeg = 0 // offset of start of current field in line
    var curEnd = 0 // current character we're on in line
    var inQoff = 0 // offset if there is a quote character to strip

    val carr = line.toCharArray // line as character array
    val slen = carr.length      // length of line

    while (curEnd < slen) {
      val chr = carr(curEnd) // get current character

      if (chr == quoteChar) { // handle a quote
        if (stripQuote) {
          if (inQ)
            inQoff = 1 // we're exiting a quoted field
          else
            curBeg = curEnd + 1 // we're starting a quoted field
        }
        inQ = !inQ
      }

      if (!inQ && chr == separator) { // we're not in quoted field & we hit a separator
        result += String.valueOf(carr, curBeg, curEnd - curBeg - inQoff)
        inQoff = 0
        curBeg = curEnd + 1 // start a new field
        curFld += 1
      }

      curEnd += 1 // move forward a character
    }

    // handle final field, may/not be terminated with separChar
    if (curBeg < slen) {
      inQoff = if (carr(curEnd - 1) == quoteChar && stripQuote) 1 else 0
      result += String.valueOf(carr, curBeg, curEnd - curBeg - inQoff)
    }

    result.toArray
  }
}

case class InvalidCSVError(cause: String) extends Exception {
  override def getMessage: String = cause

  override def fillInStackTrace(): Throwable = this
}
