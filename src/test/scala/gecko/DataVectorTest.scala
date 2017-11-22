package gecko

import syntax._

class DataVectorTest extends TestSpec {

  behavior of "DataVector"

  it should "properly create" in {
    forAll { (vec: DataVector[Int]) =>
      vec.length should be > 0

      vec.underlying.length should be(vec.length)
    }
  }

  it should "properly map" in {
    forAll { (vec: DataVector[Int]) =>
      val add1: Int => Int = (v: Int) => v + 1
      val minus1 = (v: Int) => v - 1
      val res = vec.map(add1).map(minus1)

      res.underlying should be(vec.underlying)
    }
  }

  it should "properly at" in {
    forAll { (vec: DataVector[Int], i: Int) =>
      val res = vec.at(i)
      if (0 <= i && i < vec.length && geckoInt.nonEmpty(vec.underlying(i))) res shouldBe defined
      else res shouldBe None
    }
  }

  it should "properly flatMap" in {
    forAll { (vec: DataVector[Int]) =>
      val add1 = (v: Int) => Array(v + 1).toDataVector
      val minus1 = (v: Int) => Array(v - 1).toDataVector
      val res = vec.flatMap(add1).flatMap(minus1)

      res.underlying should be(vec.underlying)
    }
  }

  it should "properly semiFlatMap" in {
    forAll { (vec: DataVector[Int]) =>
      val add1 = (v: Int) => Array(v + 1)
      val minus1 = (v: Int) => Array(v - 1)
      val res = vec.semiFlatMap(add1).semiFlatMap(minus1)

      res.underlying should be(vec.underlying)
    }
  }

  it should "properly shift" in {
    forAll { (vec: DataVector[Int], s: Int) =>
      val shift = s % (vec.length - 1)
      whenever(-vec.length < shift && shift < vec.length && shift != 0) {
        val res = vec.shift(shift)
        if (s < 0) (0 until res.length + shift).foreach { idx =>
          vec.unsafeAt(idx - shift) should be(res.unsafeAt(idx))
        } else {
          (0 until res.length - shift).foreach { idx =>
            vec.unsafeAt(idx) should be(res.unsafeAt(idx + shift))
          }
        }
      }
    }
  }

  it should "properly append" in {
    forAll { (a: DataVector[Int], b: DataVector[Int]) =>
      (a ++ b).length should be(a.length + b.length)
    }
  }

}
