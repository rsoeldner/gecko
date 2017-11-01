package gecko

sealed trait EmptyPrint[A] {
  val repr: String
}


trait EmptyPrintInstances {
  implicit val emptyPrintInt = new EmptyPrint[Int] {
    val repr: String = ""
  }

  implicit val emptyPrintLong = new EmptyPrint[Long] {
    val repr: String = ""
  }

  implicit val emptyPrintFloat = new EmptyPrint[Float] {
    val repr: String = Float.NaN.toString
  }

  implicit val emptyPrintDouble = new EmptyPrint[Double] {
    val repr: String = Double.NaN.toString
  }

  implicit val emptyPrintBoolean = new EmptyPrint[Boolean] {
    val repr: String = ""
  }

  implicit val emptyPrintString = new EmptyPrint[String] {
    val repr: String = ""
  }
}