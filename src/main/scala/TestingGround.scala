import gecko._

object TestingGround {

  def main(args: Array[String]): Unit = {
    println(DataVector(1, 2, 3, 4, 5).shiftWithFill(-2, _ + 1))

    val frame = CDataFrame.fill(10, DataVector("hold tight asnee", "he's got a pumpee"))

    frame.printPretty

    frame.mapRowAt(1, _ => "kekistani").printPretty

    frame.mapColAt(1, _ => "lord").printPretty

  }
}
