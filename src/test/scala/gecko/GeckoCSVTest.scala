package gecko
import gecko.csv._
import cats.effect.IO
import fs2._

class GeckoCSVTest extends TestSpec {

  behavior of "GeckoCSVUtil"

  it should "parse a csv properly" in {
    val csvFrameBytes =
      """1,2,3,4
        |4,5,6,7
        |8,9,10,11
      """.stripMargin.trim.getBytes("UTF-8")

    val frame = GeckoCSVUtil.parseFrame(Stream.emits(csvFrameBytes).covary[IO]).unsafeRunSync()

    frame.numRows shouldBe 3
    frame.numCols shouldBe 4
    frame.unsafeRowAtIx(0).map(_.toInt) shouldBe DataVector(1, 2, 3, 4)
  }

  it should "fail a csv with uneven rows" in {
    val csvFrameBytes =
      """1,2,3,4
        |4,5,6,7
        |8,9,10,11,12
      """.stripMargin.trim.getBytes("UTF-8")

    val frame = GeckoCSVUtil.parseFrame(Stream.emits(csvFrameBytes).covary[IO]).attempt.unsafeRunSync()

    frame shouldBe Left(DataFrameInitError("Invalid length. DataVectors must all be of the same length"))
  }

}
