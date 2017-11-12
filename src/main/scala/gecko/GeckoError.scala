package gecko


sealed trait GeckoError extends Exception

case class DataFrameInitError(cause: String) extends GeckoError {
  override def getMessage: String = cause

  override def fillInStackTrace(): Throwable = this
}


case class InvalidCSVError(cause: String) extends Exception {
  override def getMessage: String = cause

  override def fillInStackTrace(): Throwable = this
}
