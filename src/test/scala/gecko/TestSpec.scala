package gecko
import cats.Eq
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

trait TestSpec extends FlatSpec with Matchers with PropertyChecks {

  implicit def eqDataMatrix[A] = new Eq[DataMatrix[A]] {
    override def eqv(x: DataMatrix[A], y: DataMatrix[A]) = (x zip y) forall {
      case (a, b) => a.underlying.sameElements(b.underlying)
    }

  }

  implicit def eqInstaceGeckoError[T] = new Eq[GeckoError] {
    def eqv(x: GeckoError, y: GeckoError) = true
  }

  implicit val arbitraryDataVector: Arbitrary[DataVector[Int]] = Arbitrary {
    for {
      n    <- Gen.choose(1, 1000)
      list <- Gen.listOfN(n, Gen.choose(0, n))
    } yield DataVector.fromArray(list.toArray)
  }

  implicit val arbitaryFrameIndex: Arbitrary[FrameIndex[Int]] = Arbitrary {
    for {
      n    <- Gen.choose(1, 1000)
      list <- Gen.listOfN(n, Gen.choose(0, n))
    } yield FrameIndex.fromSeq(list)
  }

  implicit val arbitraryDataFrame: Arbitrary[DataFrame[Int, Int, Int]] = Arbitrary {
    for {
      rowIx <- arbitaryFrameIndex.arbitrary
      colIx <- arbitaryFrameIndex.arbitrary
      n = rowIx.length * colIx.length
      values <- Gen.listOfN[Int](n, Gen.choose(0, 100))
      mat = DataMatrix.unsafeFromArrayWithDim(rowIx.length, colIx.length, values.toArray)
    } yield DataFrame[Int, Int, Int](rowIx, colIx, mat)
  }

}
