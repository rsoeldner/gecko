package gecko


class CDataFrameTest extends TestSpec {

  behavior of "CDataFrame"

  it should "concat correct" in {
    val res = CDataFrame.fill(10, DataVector("A", "B")) ++ CDataFrame.fill(10, DataVector("C"))
    assert(res.numRows == 3 && res.numCols == 10)
  }

}
