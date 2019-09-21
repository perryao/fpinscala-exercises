package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0X5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n       = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {
  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  // exercise 6.7
  // from answerkey: https://github.com/fpinscala/fpinscala/blob/master/answerkey/state/07.answer.scala
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // original solution by me
  def mysequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      // elements in fs are functions rng => (A, RNG)
      // return (List[A], RNG)
      fs.foldLeft((List[A](), rng)) { (acc, rand) =>
        val (xs, r1) = acc
        val (x, r2)  = rand(r1)
        (xs :+ x, r2)
      }
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
    // abs doesn't work when nextInt returns
    // Int.MinValue since Int.MinValue doesn't
    // have a non-negative counterpart
    // (i.abs, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // exercise 6.5
  def _double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def doubleByMap(rng: RNG): (Double, RNG) = _double(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // tail-recursive solution from https://github.com/fpinscala/fpinscala/blob/master/answerkey/state/04.answer.scala
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count <= 0) (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    }

    go(count, rng, List())
  }

  // my solution using foldleft
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (List(), rng)
    else {
      val (i, r) = rng.nextInt
      val z      = (List(i), r)
      (0 until count).foldLeft(z)({
        case ((xs, r), _) =>
          val (x, r1) = r.nextInt
          (x :: xs, r1)
      })
    }
  }
}
