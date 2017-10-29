package gecko

import org.scalatest.{FlatSpec, Matchers}
import syntax._

class DataVectorTest extends FlatSpec with Matchers {

  behavior of "DataVector"

  it should "map correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.map(_ + 10).underlying shouldBe(Array(11,12,13,14,15))
  }

  it should "flatmap correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.flatMap(x => Array(x + 10).toDataVector).underlying shouldBe(Array(11,12,13,14,15))
  }

  it should "shift up correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.shift(2).underlying shouldBe(Array(Int.MinValue,Int.MinValue,1,2,3))
  }

  it should "shift down correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.shift(-2).underlying shouldBe(Array(3,4,5,Int.MinValue, Int.MinValue))
  }

}
