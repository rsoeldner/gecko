package gecko
import org.scalacheck._

class DataFrameTest extends TestSpec {
  import Prop.{forAll}

  behavior of "DataFrame"

  it should "construct properly" in {
    forAll { df: DataFrame[Int, Int, Int] =>
      df.numRows should be > 0
      df.numCols should be > 0

      df.values.length should equal(df.numRows)
      df.values.foreach(_.length should equal(df.numCols))
    }
  }

  it should "concat properly" in {
    forAll { (a: DataFrame[Int, Int, Int]) =>
      val df = (a concat a)

      df should be('Right)
      df.right.get.numRows should equal(a.numRows + a.numRows)
    }
  }

  it should "drop column by index" in {
    forAll { (frame: DataFrame[Int, Int, Int], c: Int) =>
      val result: Either[GeckoError, DataFrame[Int, Int, Int]] = frame.dropColByIx(c)
      if (0 <= c && c < frame.numCols) {
        result should be('Right)
        result.right.get.numCols should equal(frame.numCols - 1)
      } else result should be('Left)
    }
  }
}
