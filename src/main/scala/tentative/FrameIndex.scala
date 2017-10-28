package tentative

final case class FrameIndex[@specialized(Int, Double, Boolean, Long) A](
    underlying: Array[A],
    private[tentative] val indexes: Array[Int]
)
