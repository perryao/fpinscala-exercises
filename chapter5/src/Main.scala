package chapter5

sealed trait Stream[+A] {
  import Stream._
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def headOptionByFoldRight: Option[A] =
    foldRight(None:Option[A])((h, _) => Some(h))

  // susceptible to stackoverflow.
  // TODO: use tail recursion
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _          => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def takeWhileByFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def existsByFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def forAll(p: A => Boolean): Boolean = !exists(a => !p(a))
  // The book is looking for this, which uses foldRight. Probably
  // more clear than the above
  def forAllByFoldRight(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def find(f: A => Boolean): Option[A] =
    filter(f).headOption

  def append[B>:A](a: => Stream[B]): Stream[B] =
    foldRight(a)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // see https://github.com/fpinscala/fpinscala/blob/master/answerkey/laziness/08.answer.scala
  // for a more efficient version using a lazy val
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantByUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def onesByUnfold(): Stream[Int] = unfold(1)(_ => Some(1, 1))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def fromByUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def fibs(): Stream[Int] = {
    def gen(prev: Int, cur: Int): Stream[Int] = cons(prev, gen(cur, prev + cur))
    gen(0, 1)
  }

  // read more on Pattern Matching anonymous functions here: https://www.scala-lang.org/files/archive/spec/2.12/08-pattern-matching.html#pattern-matching-anonymous-functions
  def fibsByUnfold(): Stream[Int] = unfold((0,1)) {case (f0, f1) => Some(f0, (f1, f0 + f1))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f)) 
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object Main extends App {
  // exercise 5.1
  println(Stream(1, 2, 3).toList) // List(1,2,3)

  // exercise 5.2
  println(Stream(1, 2, 3).take(2).toList) // List(1,2)
  println(Stream(1, 2, 3).drop(2).toList) // List(3)

  // exercise 5.3
  println(Stream(1, 2, 3).takeWhile(_ < 3).toList) // List(1,2)

  // exercise 5.4
  println(Stream(1, 2, 3).forAll(_ > 0)) // true
  println(Stream(1, 2, 3).forAllByFoldRight(_ > 0)) // true
  println(Stream(1, 2, 3).forAll(_ < 2)) // false
  println(Stream(1, 2, 3).forAllByFoldRight(_ < 2)) // false

  // exercise 5.5
  println(Stream(1, 2, 3).takeWhileByFoldRight(_ < 3).toList) // List(1,2)

  // exercise 5.6
  println(Stream(1,2,3).headOptionByFoldRight) // Some(1)
  println(Stream().headOptionByFoldRight) // None

  // exercise 5.7
  println(Stream(1,2,3).map(_ + 1).toList) // List(2,3,4)
  println(Stream(1,2,3).filter(_ < 3).toList) // List(1,2)
  println(Stream(1,2,3).append(Stream(3,4,5)).toList) // List(1,2,3,4,5)
  println(Stream(1, 2, 3).flatMap(x => Stream(x, x + 1)).toList) // List(1,2,2,3,3,4)
  println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 ==0).toList)

  // exercise 5.8
  println(Stream.constant(5).take(3).toList) // List(5,5,5)

  // exercise 5.9
  println(Stream.from(5).take(3).toList) // List(5,6,7)

  // exercise 5.10
  println(Stream.fibs().take(7).toList) // List(0,1,1,2,3,5,8

  // exercise 5.11
  // TODO: determine the feature of scala that implicitly converts Some(x, x + 1) to Some((x, x + 1))
  // e.g. to a tuple
  println(Stream.unfold(0)(x => Some(x, x + 1)).take(3).toList) // List(0,1,2)

  // exercise 5.12
  println(Stream.fibsByUnfold().take(7).toList) // List(0,1,1,2,3,5,8
  println(Stream.fromByUnfold(5).take(3).toList) // List(5,6,7)
  println(Stream.constantByUnfold(5).take(3).toList) // List(5,5,5)
  println(Stream.onesByUnfold().take(3).toList) // List(1,1,1)
}