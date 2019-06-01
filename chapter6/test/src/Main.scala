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

  it should "return a positive value" in {
    import SimpleRNG._ 
    val seeds = Table(
      ("n"),
      -1,
      0,
      1,
      Int.MinValue,
      Int.MaxValue,
      42,
      10000,
    )
    forAll(seeds) { (n: Int) =>
      val rng = SimpleRNG(n)
      val (n1, rng2) = nonNegativeInt(rng)
      assert((0 to Int.MaxValue) contains n1)
    }
  }

  it should "return positive when nextInt returns Int.MinValue" in {
    import SimpleRNG._ 
    val rng = new RNG {
      def nextInt: (Int, RNG) = (Int.MinValue, SimpleRNG(0))
    }
    val (n1, rng2) = nonNegativeInt(rng)
    assert((0 to Int.MaxValue) contains n1)
  }
}
