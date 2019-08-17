package chapter2

import annotation._
import scalaz.Memo

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
      if (n >= as.length - 1) true                    // made it to  end of as, so must be sorted
      else if (ordered(as(n), as(n + 1))) loop(n + 1) // if ordered, loop again at the next index
      else false                                      // not ordered
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
  val add2         = curriedAdder(2)
  println(add2(2))

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  val uncurriedAdder = uncurry(curriedAdder)
  println(uncurriedAdder(1, 1))

  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  val subtracter        = (a: Int, b: Int) => b - a
  val curriedSubtracter = curry(subtracter)
  val subtract1         = curriedSubtracter(1)
  println(compose(add2, subtract1)(5)) // 6
}
