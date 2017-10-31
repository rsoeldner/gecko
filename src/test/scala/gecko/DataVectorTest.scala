package gecko

import syntax._

class DataVectorTest extends TestSpec {

  behavior of "DataVector"

  it should "map correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.map(_ + 10).underlying shouldBe(Array(11,12,13,14,15))
  }

  it should "flatmap correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.flatMap(x => Array(x + 10).toDataVector).underlying shouldBe(Array(11,12,13,14,15))
  }

  it should "semiFlatMap correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.semiFlatMap(x => Array(x + 10)).underlying shouldBe(Array(11,12,13,14,15))
  }

  it should "shift up correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.shift(2).underlying shouldBe(Array(Int.MinValue,Int.MinValue,1,2,3))
  }

  it should "shift down correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5))
    vec.shift(-2).underlying shouldBe(Array(3,4,5,Int.MinValue, Int.MinValue))
  }

  it should "append correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5)) ++ Array(6).toDataVector
    vec.underlying shouldBe(Array(1,2,3,4,5,6))
  }

  it should "drop correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5)).drop(2)
    vec.underlying shouldBe(Array(3,4,5))
  }

  it should "dropLast correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5)).dropLast
    vec.underlying shouldBe(Array(1,2,3,4))
  }

  it should "dropLastN correct" in {
    val vec = DataVector.fromArray(Array[Int](1,2,3,4,5)).dropLastN(2)
    vec.underlying shouldBe(Array(1,2,3))
  }


}
