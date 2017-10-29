package gecko

import org.scalatest.{FlatSpec, Matchers}


class DataVectorTest extends FlatSpec with Matchers {

  behavior of "DataVector"

  it should "shift up correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.shift(2).underlying shouldBe(Array(Int.MinValue,Int.MinValue,1,2,3))
  }

  it should "shift down correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.shift(-2).underlying shouldBe(Array(3,4,5,Int.MinValue, Int.MinValue))
  }

}
