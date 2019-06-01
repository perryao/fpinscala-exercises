import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._
import chapter6._

class MainSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  it should "be a pure random number generator" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    assert(n1 == 16159453)
  }

  it should "return a value between 0 and Int.MaxInt" in {
    import SimpleRNG._ 
    val minValueRNG = new RNG {
      def nextInt: (Int, RNG) = (Int.MinValue, SimpleRNG(0))
    }
    val rngs = Table(
      ("rng"),
      SimpleRNG(-1),
      SimpleRNG(0),
      SimpleRNG(1),
      SimpleRNG(Int.MinValue),
      SimpleRNG(Int.MaxValue),
      SimpleRNG(42),
      SimpleRNG(10000),
      minValueRNG
    )
    forAll(rngs) { (rng: RNG) =>
      val (n1, rng2) = nonNegativeInt(rng)
      assert((0 to Int.MaxValue) contains n1)
    }
  }
}
