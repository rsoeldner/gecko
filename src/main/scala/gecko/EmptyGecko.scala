package gecko

sealed trait EmptyGecko[A] {
  val emptyElement: A
  def nonEmpty(a: A): Boolean
}

trait EmptyGeckoInstances {
  implicit val geckoInt: EmptyGecko[Int] = new EmptyGecko[Int] {
    val emptyElement: Int = Int.MinValue

    def nonEmpty(a: Int): Boolean = a != Int.MinValue
  }

  implicit val geckoLong: EmptyGecko[Long] = new EmptyGecko[Long] {
    val emptyElement: Long = Long.MinValue

    def nonEmpty(a: Long): Boolean = a != Long.MinValue
  }

  implicit val geckoDouble: EmptyGecko[Double] = new EmptyGecko[Double] {
    val emptyElement: Double = Double.NaN

    def nonEmpty(a: Double): Boolean = a != Double.NaN
  }

  implicit val geckoBool: EmptyGecko[Boolean] = new EmptyGecko[Boolean] {
    val emptyElement: Boolean = false

    def nonEmpty(a: Boolean): Boolean = a
  }

  implicit val geckoString: EmptyGecko[String] = new EmptyGecko[String] {
    val emptyElement =""

    def nonEmpty(a: String) = !a.isEmpty
  }

  implicit final def productEmptyGeckerino[A, B](implicit empty1: EmptyGecko[A], empty2: EmptyGecko[B]): EmptyGecko[(A, B)] = new EmptyGecko[(A, B)] {
    def nonEmpty(a: (A, B)): Boolean = empty1.nonEmpty(a._1) && empty2.nonEmpty(a._2)

    val emptyElement: (A, B) = (empty1.emptyElement, empty2.emptyElement)
  }
}
