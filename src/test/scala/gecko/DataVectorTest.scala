package gecko

import syntax._

class DataVectorTest extends TestSpec {

  behavior of "DataVector"

  it should "properly create" in {
    forAll { (vec: DataVector[Int]) =>
      vec.length should be > 0

      vec.underlying.length should be(vec.length)
      vec should be(vec)
    }
  }

  it should "properly map" in {
    forAll { (vec: DataVector[Int]) =>
      val add1 = (v: Int) => v + 1
      val res  = vec.map(add1)

      res.underlying should be(vec.underlying.map(_ + 1))
    }
  }

  it should "properly flatMap" in {
    forAll { (vec: DataVector[Int]) =>
      val add1 = (v: Int) => Array(v + 1).toDataVector
      val res  = vec.flatMap(add1)

      res.underlying should be(vec.underlying.map(_ + 1))
    }
  }

  it should "properly semiFlatMap" in {
    forAll { (vec: DataVector[Int]) =>
      val add1 = (v: Int) => Array(v + 1)
      val res  = vec.semiFlatMap(add1)

      res.underlying should be(vec.underlying.map(_ + 1))
    }
  }

  it should "properly shift" in {
    forAll { (vec: DataVector[Int], s: Int) =>
      whenever(-vec.length < s && s < vec.length && s != 0) {
        val res = vec.shift(s)
        if (s < 0) (0 until res.length + s).foreach { idx =>
          vec.unsafeAt(idx - s) should be(res.unsafeAt(idx))
        } else {
          (0 until res.length - s).foreach { idx =>
            vec.unsafeAt(idx) should be(res.unsafeAt(idx + s))
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
