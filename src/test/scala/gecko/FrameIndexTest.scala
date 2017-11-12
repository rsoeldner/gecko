package gecko

class FrameIndexTest extends TestSpec {

  behavior of "FrameIndex"

  it should "default should have expected size" in {
    val res = FrameIndex.default(10)
    assert(res.length == 10)
  }

  it should "concat properly" in {
    val res = FrameIndex.default(10) ++ FrameIndex.default(5)
    assert(res.length == 15)
  }

  it should "store properly" in {
    val res = FrameIndex.default(10).unsafeSlice(5,10)
    assert(res.length == 5)
  }

  it should "remove properly" in {
    val res = FrameIndex.default(10).unsafeRemoveIx(0)
    assert(res.length == 9)
  }

}
