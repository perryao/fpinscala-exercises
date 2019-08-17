import chapter6._
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._
import SimpleRNG._

class MainSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  "SimpleRNG" should "be a pure random number generator" in {
    val rng        = SimpleRNG(42)
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

  // TODO: what would a good test be?
  "intDouble" should "generate an int and a double" in new SimpleRNGFixtures {
    forAll(rngs) { rng =>
      val ((n1, n2), _) = intDouble(rng)
      println(n1, n2)
    }
  }

  "doubleInt" should "generate a double and int" in new SimpleRNGFixtures {
    forAll(rngs) { rng =>
      val ((n1, n2), _) = doubleInt(rng)
      println(n1, n2)
    }
  }

  "double3" should "generate 3 doubles" in new SimpleRNGFixtures {
    forAll(rngs) { rng =>
      val ((n1, n2, n3), _) = double3(rng)
      println(n1, n2, n3)
    }
  }

  "ints" should "return an empty list for count 0" in new SimpleRNGFixtures {
    forAll(rngs) { rng =>
      val (randomInts, _) = ints2(0)(rng)
      println(randomInts)
      assert(randomInts.length == 0)
    }
  }

  "ints" should "generate a list of ints" in new SimpleRNGFixtures {
    forAll(rngs) { rng =>
      val (randomInts, _) = ints2(5)(rng)
      println(randomInts)
    }
  }
}
