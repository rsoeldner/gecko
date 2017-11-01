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

  implicit val emptyGeckoAny: EmptyGecko[Any] = new EmptyGecko[Any] {
    val emptyElement: Any = null

    def nonEmpty(a: Any): Boolean = a != null
  }

  implicit final def emptyGeckerino[A]: EmptyGecko[A] = emptyGeckoAny.asInstanceOf[EmptyGecko[A]]
}
