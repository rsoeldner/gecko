import matrix.RowMajorMatrix

object Main {

  def main(args: Array[String]): Unit = {

    val mat = new RowMajorMatrix[Float](3, 5)
    mat.fillWith(1)

    println(mat.toRawString)

    print(mat)

  }

}
