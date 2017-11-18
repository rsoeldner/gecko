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

  it should "default should have expected size" in {
    val res = FrameIndex.default(10)
    assert(res.length == 10)
  }

  it should "store properly" in {
    val res = FrameIndex.default(10).unsafeSlice(5, 10)
    assert(res.length == 5)
  }

  it should "remove properly" in {
    val res = FrameIndex.default(10).unsafeDropIx(0)
    assert(res.length == 9)
  }

}
