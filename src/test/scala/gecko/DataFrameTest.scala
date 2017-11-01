package gecko

class DataFrameTest extends TestSpec {

  behavior of "DataFrame"

  it should "groupBy correct" in {
    val frame =
      DataFrame.fill(10, DataVector("A", "B", "C")) ++
      DataFrame.fill(10, DataVector("B", "A", "C")) ++
      DataFrame.fill(10, DataVector("B", "D", "C"))

    val result = frame.groupBy(List(0, 1))

    result should have length (3)
  }

}
