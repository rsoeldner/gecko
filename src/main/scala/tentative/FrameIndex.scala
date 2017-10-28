package tentative

import scala.reflect.ClassTag

final case class FrameIndex[@specialized(Int, Double, Boolean, Long) A: ClassTag](
    underlying: Array[A],
    private[tentative] val indexes: Array[Int]
) {
    @inline def length: Int = underlying.length

    def slice(begin: Int, end: Int): FrameIndex[A] = {
        val underlyingSlice = copyRange(underlying, begin, end)
        val indexSlice = copyRange(indexes, begin, end)
        FrameIndex(underlyingSlice, indexSlice)
    }
}

object FrameIndex {
    def default(size: Int): FrameIndex[Int] = {
        val ix = Array.range(0, size)
        FrameIndex(ix, ix)
    }
}
