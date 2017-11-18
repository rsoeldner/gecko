package gecko
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

class DataFrameTest extends TestSpec with PropertyChecks {
  import Prop.forAll

  behavior of "DataFrame"


  implicit val arbitaryFrameIndex: Arbitrary[FrameIndex[Int]] = Arbitrary {
    for {
      n      <- Gen.choose(1, 1000)
      length <- Gen.listOfN(n - 1, Gen.choose(0, n))
    } yield FrameIndex.fromSeq(length)
  }

  implicit val arbitraryFrame: Arbitrary[DataFrame[Int, Int, Int]] = Arbitrary {
    for {
      nRows   <- Gen.choose(1, 1000)
      nCols   <- Gen.choose(1, 1000)
      rvalues <- Gen.listOfN[Int](nRows, Gen.choose(0, nRows))
      cvalues <- Gen.listOfN[Int](nCols, Gen.choose(0, nCols))
      ridx = FrameIndex.fromSeq(rvalues)
      cidx = FrameIndex.fromSeq(cvalues)
      n    = nRows * nCols
      matrix <- Gen.listOfN[Int](n, Gen.choose(0, 100))
    } yield DataFrame[Int, Int, Int](ridx, cidx, DataMatrix.unsafeFromArrayWithDim(nRows, nCols, matrix.toArray))
  }

  it should "construct properly" in {
    forAll { df: DataFrame[Int, Int, Int] =>
      df.numRows > 0
      df.numCols > 0

      df.values.length == df.numRows
      df.values.forall(_.length == df.numCols)
    }
  }

  it should "drop col" in {
    forAll { (frame: DataFrame[Int, Int, Int]) =>
      frame.colIx.underlying.foreach { idx =>
        frame.colIx.findOne(idx) shouldBe Right(idx)
      }
    }
  }

  it should "rowAtIx" in {
    forAll { (frame: DataFrame[Int, Int, Int], r: Int) =>
      val res = frame.rowAtIx(r)
      if (0 > r || r >= frame.numRows) res shouldBe 'Left
      else res shouldBe 'Right
    }
  }

  it should "colAtIx" in {
    forAll { (frame: DataFrame[Int, Int, Int], c: Int) =>
      val res = frame.colAtIx(c)
      if (0 > c || c >= frame.numCols) res shouldBe 'Left
      else res shouldBe 'Right
    }
  }

  it should "groupBy correct" in {
    val frame =
      DataFrame.fill(10, DataVector("A", "B", "C")) ++
        DataFrame.fill(10, DataVector("B", "A", "C")) ++
        DataFrame.fill(10, DataVector("B", "D", "C"))

    val result = frame.groupBy(List(0, 1))

    result should have length (3)
  }

}
