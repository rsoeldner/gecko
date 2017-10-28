import frame.Frame
import matrix.RowMajorMatrix

object Main {

  def main(args: Array[String]): Unit = {

    //    val mat = new RowMajorMatrix[Float](3, 5)
    //    mat.fillWith(1)
    //
    //    print(mat)
    //
    //    val res = RowMajorMatrix.from(
    //      Array(
    //        Array(0, 1, 2),
    //        Array(3, 4, 5)
    //      ))
    //
    //    print(res)
    //  }




    val frame = Frame.from(
      Array(
        Array("A", "1", "A"),
        Array("B", "2", "A"),
        Array("C", "3", "B"),
      )
    )

    println(frame)

  }

}
