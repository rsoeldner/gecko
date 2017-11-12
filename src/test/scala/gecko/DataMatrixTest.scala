package gecko
import cats.implicits._

class DataMatrixTest extends TestSpec {
  behavior of "DataMatrix"

  it should "create Matrix from single array" in {
    val values = Array(1, 2, 3, 10, 20, 30, 100, 200, 300)
    val matrix = DataMatrix.fromArrayWithDim(3, 3, values)

    assert(
      matrix === DataMatrix.fromArray(Array(DataVector(1, 2, 3), DataVector(10, 20, 30), DataVector(100, 200, 300)))
    )
  }

  it should "transpose correct" in {
    val matrix = DataMatrix.unsafeFromArray(Array(DataVector(1,2,3), DataVector(10,20,30)))
    val transp = DataMatrix.unsafeFromArray(Array(DataVector(1,10), DataVector(2,20), DataVector(3,30)))
    matrix.transpose shouldBe transp
  }

}
