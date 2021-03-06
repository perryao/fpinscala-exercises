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

  "doubleByMap" should "generate a double between 0 and 1, not including 1" in new SimpleRNGFixtures {
    forAll(rngs) { (rng: RNG) =>
      val (n1, _) = doubleByMap(rng)
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

  "map2" should "map Rand[A] and Rand[B] to Rand[C]" in {
    val ra              = unit(1)
    val rb              = unit(1.0)
    val rc              = map2(ra, rb)((_, _))
    val ((c1, c2), rng) = rc(SimpleRNG(1))
    assert(c1 == 1 && c2 == 1.0)
  }

  "sequence" should "combine a list of transitions into a single transition" in {
    val r = SimpleRNG(1)
    val l = sequence(List(unit(1), unit(2), unit(3)))(r)._1
    assert(l == List(1, 2, 3))
  }

  // exercise 6.7
  "flatMap" should "map Rand[A] to Rand[B]" in {
    val rng             = SimpleRNG(1)
    val a: Rand[Int]    = unit(1)
    val b: Rand[String] = flatMap(a)(a => unit(a.toString()))
    val (aValue, r1)    = a(rng)
    val (bValue, r2)    = b(r1)
    assert(bValue == "1")
  }

  // exercise 6.7
  "nonNegativeLessThan" should "generate ints less than n" in {
    val (a, rng) = nonNegativeLessThan(4)(SimpleRNG(4))
    assert(a < 4)
  }
}
