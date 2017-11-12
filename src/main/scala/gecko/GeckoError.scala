package gecko

sealed trait GeckoError extends Exception

case class DataFrameInitError(cause: String) extends GeckoError {
  override def getMessage: String = cause
  override def fillInStackTrace(): Throwable = this
}

case class InvalidCSVError(cause: String) extends GeckoError {
  override def getMessage: String = cause
  override def fillInStackTrace(): Throwable = this
}

case class RowOutOfBoundError(requested: Int, bound: Int) extends GeckoError {
  override def getMessage: String            = s"Requested row-index: $requested out of bound ($bound)"
  override def fillInStackTrace(): Throwable = this
}

case class ColOutOfBoundError(requested: Int, bound: Int) extends GeckoError {
  override def getMessage: String            = s"Requested col-index: $requested out of bound ($bound)"
  override def fillInStackTrace(): Throwable = this
}

case class ElementNotFoundError[A](identifier: A) extends GeckoError {
  override def getMessage: String            = s"Could not found element '$identifier"
  override def fillInStackTrace(): Throwable = this
}

case class IndexOutOfBoundError(index: Int, bound: Int) extends GeckoError {
  override def getMessage: String            = s"Index $index out of bound $bound"
  override def fillInStackTrace(): Throwable = this
}

case class NotEnoughElementsError(requested: Int, availible: Int) extends GeckoError {
  override def getMessage: String            = s"Not enough elements, requested: $requested but availible: $availible"
  override def fillInStackTrace(): Throwable = this
}

case object InvalidArgumentError extends GeckoError {
  override def getMessage: String            = s"Invalid arguments"
  override def fillInStackTrace(): Throwable = this
}
