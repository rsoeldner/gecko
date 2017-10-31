package gecko

class FrameIndexTest extends TestSpec {

  behavior of "FrameIndex"

  it should "construct properly" in {
    val res = FrameIndex.default(10)
    assert(res.length == 10)
  }

  it should "concat properly" in {
    val res = FrameIndex.default(10) ++ FrameIndex.default(5)
    assert(res.length == 15)
  }

}
