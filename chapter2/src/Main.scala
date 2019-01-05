package chapter2

import annotation._
import scalaz.Memo

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, xs) => xs
    case _ => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Cons(x, xs) => Cons(a, xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // returns all but the last element of a list
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t)) 
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Main extends App {
  // expensive, and not tail recursive
  def fib(n: Int): Int = {
    def go(n: Int): Int =
      if (n == 0 || n == 1) n
      else go(n - 1) + go(n - 2)
    go(n)
  }

  // Tail recursive and memoized with scalaz
  val fibTail: Int => Int = Memo.mutableHashMapMemo {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = n match {
      case 0 => prev
      case _ => loop(n - 1, cur, prev + cur)
    }
    // rather than pattern matching, we can also do this inside loop
    // if (n == 0) prev
    // else loop(n - 1, cur, prev + cur)
    loop(_, 0, 1)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true // made it to  end of as, so must be sorted
      else if (ordered(as(n), as(n + 1))) loop(n + 1) // if ordered, loop again at the next index
      else false // not ordered
    }
    loop(0)
  }

  for (i <- 0 to 46)
    println(fibTail(i))

  println(isSorted[Int](Array(1, 2, 3), _ < _)) // true
  println(isSorted[Int](Array(3, 2, 1), _ < _)) // false
  println(isSorted[Int](Array(3, 2, 1), _ > _)) // true
  println(isSorted[Int](Array(2, 1, 3), _ < _)) // false
  println(isSorted[Int](Array(2, 3, 1), _ < _)) // false


  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  val adder = (a: Int, b: Int) => a + b
  
  val curriedAdder = curry(adder)
  val add2 = curriedAdder(2)
  println(add2(2))

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  val uncurriedAdder = uncurry(curriedAdder)
  println(uncurriedAdder(1, 1))


  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  val subtracter = (a: Int, b: Int) => b - a
  val curriedSubtracter = curry(subtracter)
  val subtract1 = curriedSubtracter(1)
  println(compose(add2, subtract1)(5)) // 6

  // exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this is the matching case
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(x)

  // exercise 3.2
  println(List.tail(List(1, 2, 3, 4)))

  // exercise 3.3
  println(List.setHead(1, List(2, 3, 4)))

  // exercise 3.4
  println(List.drop(List(1,2,3,4), 2))

  // exercise 3.5
  println(List.dropWhile[Int](List(1, 2, 3, 4), _ < 3))

  // exercise 3.6
  println(List.init(List(1, 2, 3, 4, 5)))

  // exercise 3.7 Can product2 short circuit if a 0 is anywhere in the list?
  // The answer is it can't as implemented. It will always traverse the full list.
  println(List.product2(List(1, 0, 3, 4)))
}
