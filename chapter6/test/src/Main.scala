import chapter6._
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._
import SimpleRNG._

class MainSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  "SimpleRNG" should "be a pure random number generator" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    assert(n1 == 16159453)
  }

  trait SimpleRNGFixtures {
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
  }

  "nonNegativeInt" should "return a value between 0 and Int.MaxInt" in new SimpleRNGFixtures {
    forAll(rngs) { (rng: RNG) =>
      val (n1, _) = nonNegativeInt(rng)
      assert((0 to Int.MaxValue) contains n1)
    }
  }

  "double" should "generate a double between 0 and 1, not including 1" in new SimpleRNGFixtures {
    forAll(rngs) { (rng: RNG) =>
      val (n1, _) = double(rng)
      assert(n1 >= 0 && n1 < 1)
    }
  }
}
