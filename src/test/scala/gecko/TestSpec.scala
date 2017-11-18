package gecko
import cats.Eq
import org.scalatest.{FlatSpec, Matchers}

trait TestSpec extends FlatSpec with Matchers{

  implicit def eqDataMatrix[A] = new Eq[DataMatrix[A]] {
    override def eqv(x: DataMatrix[A], y: DataMatrix[A]) = (x zip y) forall {
      case (a,b) => a.underlying.sameElements(b.underlying)
    }

  }

  implicit def eqInstaceGeckoError[T] = new Eq[GeckoError] {
    def eqv(x: GeckoError, y: GeckoError) = true
  }

}
