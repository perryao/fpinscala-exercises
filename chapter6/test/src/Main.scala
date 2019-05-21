import org.scalatest._
import chapter6._

class MainSpec extends FlatSpec with Matchers {
  it should "be a pure random number generator" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    assert(n1 == 16159453)
  }
}
