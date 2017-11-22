package gecko
import cats.Eq
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

trait TestSpec extends FlatSpec with Matchers with PropertyChecks with EmptyGeckoInstances {

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
      n    <- Gen.choose(0, 1000)
      list <- Gen.listOfN(n, Gen.choose(0, n))
      l = list.map(x => if(x < n/10) geckoInt.emptyElement else x)
      dvec = if(n == 0) DataVector.empty[Int] else DataVector.fromArray(l.toArray)
    } yield dvec
  }

  implicit val arbitaryFrameIndex: Arbitrary[FrameIndex[Int]] = Arbitrary {
    for {
      n    <- Gen.choose(0, 1000)
      list <- Gen.listOfN(n, Gen.choose(0, n))
      fidx = if(n == 0) FrameIndex.empty[Int] else FrameIndex.fromSeq(list)
    } yield fidx
  }

  implicit val arbitraryDataFrame: Arbitrary[DataFrame[Int, Int, Int]] = Arbitrary {
    for {
      rowIx <- arbitaryFrameIndex.arbitrary
      colIx <- arbitaryFrameIndex.arbitrary
      n = rowIx.length * colIx.length
      values <- Gen.listOfN[Int](n, Gen.choose(0, 100))
      t = values.map(x => if(x < 10) geckoInt.emptyElement else x) // 10% empty
      mat = if(n==0) DataMatrix.empty[Int] else DataMatrix.unsafeFromArrayWithDim(rowIx.length, colIx.length, t.toArray)
    } yield DataFrame[Int, Int, Int](rowIx, colIx, mat)
  }

}
