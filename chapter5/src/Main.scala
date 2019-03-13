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
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
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
}
