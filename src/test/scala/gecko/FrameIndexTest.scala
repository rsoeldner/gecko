package gecko

import org.scalacheck.Prop

class FrameIndexTest extends TestSpec {
  import Prop.forAll

  behavior of "FrameIndex"

  it should "concat properly" in {
    forAll { (a: FrameIndex[Int], b: FrameIndex[Int]) =>
      (a ++ b).length should be(a.length + b.length)
    }
  }

  it should "slice properly" in {
    forAll { (a: FrameIndex[Int], b: Int, e: Int) =>
      if (a.length != 0) {
        val begin = b.abs % a.length
        val end   = e.abs % a.length
        whenever(0 <= begin && begin < end && end < a.length) {
          val res = a.slice(begin, end)
          res should be('Right)
          res.right.get.length should be(end - begin)
        }
      }
    }
  }

  it should "remove properly" in {
    forAll { (a: FrameIndex[Int], i: Int) =>
      if (a.length != 0) {
        val idx = i.abs % a.length
        whenever(0 <= idx && idx < a.length) {
          val res = a.dropIx(idx)
          res should be('Right)
          res.right.get.length should be(a.length - 1)
        }
      }
    }
  }

  it should "find properly" in {
    forAll { (a: FrameIndex[Int]) =>
      a.underlying.foreach { v =>
        val res = a.findOne(v)
        res should be('Right)
        a.underlying(res.right.get) should be(v)
      }

    }
  }

}
