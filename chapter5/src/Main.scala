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

  def takeByUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
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

  def takeWhileByUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
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

  def mapByUnfold[B](f: A => B): Stream[B] =
    unfold(this){ 
      case Cons(h, t) => Some((f(h()), t())) 
      case _ => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def find(f: A => Boolean): Option[A] =
    filter(f).headOption

  def append[B>:A](a: => Stream[B]): Stream[B] =
    foldRight(a)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).takeWhile(!_._2.isEmpty).forAll { case (h1, h2) => h1 == h2 }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append(Stream(empty))

  // see: https://github.com/fpinscala/fpinscala/blob/master/answerkey/laziness/16.answer.scala
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. 
      // So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      println((a, p1._1, p1._2.toList))
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
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
  println(Stream.fibs().take(7).toList) // List(0,1,1,2,3,5,8)

  // exercise 5.11
  // TODO: determine the feature of scala that implicitly converts Some(x, x + 1) to Some((x, x + 1))
  // e.g. to a tuple
  println(Stream.unfold(0)(x => Some(x, x + 1)).take(3).toList) // List(0,1,2)

  // exercise 5.12
  println(Stream.fibsByUnfold().take(7).toList) // List(0,1,1,2,3,5,8
  println(Stream.fromByUnfold(5).take(3).toList) // List(5,6,7)
  println(Stream.constantByUnfold(5).take(3).toList) // List(5,5,5)
  println(Stream.onesByUnfold().take(3).toList) // List(1,1,1)

  // exercise 5.13
  println(Stream(1,2,3).mapByUnfold(_ + 1).toList) // List(2,3,4)
  println(Stream.fibs().takeByUnfold(7).toList) // List(0,1,1,2,3,5,8)
  println(Stream(1, 2, 3).takeWhileByUnfold(_ < 3).toList) // List(1,2)
  println(Stream(1,2,3).zipWith(Stream(4,5,6))(_ + _).toList) // List(5,7,9)
  println(Stream(1,2,3).zipAll(Stream(4,5)).toList) // List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None))
  println(Stream(1,2).zipAll(Stream(4,5,6)).toList) // List((Some(1), Some(4)), (Some(2), Some(5)), (None, Some(6)))

  // exercise 5.14
  println(Stream(1,2).startsWith(Stream(1))) // true

  // exercise 5.15
  println(Stream(1,2).tails.map(_.toList).toList) // List(List(1,2), List(2), List())

  // hasSubsequence
  println(Stream(1,2,3,4,5,6).hasSubsequence(Stream(2,3))) // true

  // exercise 5.16
  println(Stream(1,2,3).scanRight(0)(_ + _).toList) // List(6,5,3,0)
}
