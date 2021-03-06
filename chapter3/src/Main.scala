package chapter3

import annotation._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString(): String = s"Cons($head,$tail)"
}

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _ max _)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sumByFold(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def productByFold(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, xs) => xs
    case _           => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil         => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l
  }

  def setHead[A](a: A, as: List[A]): List[A] = as match {
    // case Nil => Cons(a, Nil) // TODO: do this or throw error?
    case Cons(x, xs) => Cons(a, xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // in terms of foldRight
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight2(a1, a2)(Cons(_, _))

  // in terms of foldLeft
  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((b, a) => Cons(a, b))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append3)

  // better would be using foldRight
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil        => Nil
    case Cons(h, t) => Cons(h + 1, addOne(t))
  }

  // better would be using foldRight
  def toStrings(l: List[Double]): List[String] = l match {
    case Nil        => Nil
    case Cons(h, t) => Cons(h.toString, toStrings(t))
  }

  // returns all but the last element of a list
  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  // implemented in terms of foldLeft
  def foldRight2[A, B](ls: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(ls, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  // TODO: this was my original implementation that seemed to work fine for addition
  // Figure out what cases it's not good enough for
  // foldLeft(ls, z)((b, a) => f(a, b))

  // originally implemented this using an internally defined loop function
  // to get tail recursion. but used the reference repo to realize that the entire
  // function itself could be defined as tail-recursive, not needing an inner function
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // implemented in terms of foldRight
  // see: https://github.com/fpinscala/fpinscala/blob/master/answerkey/datastructures/13.answer.scala#L12
  def foldLeft2[A, B](ls: List[A], z: B)(f: (B, A) => B) =
    foldRight(ls, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  def lengthByFold[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](ls: List[A]): List[A] = foldLeft(ls, List[A]())((acc, h) => Cons(h, acc))

  def map[A, B](ls: List[A])(f: A => B): List[B] =
    foldRight2(ls, Nil: List[B])((a, b) => Cons(f(a), b))

  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] = concat(map(ls)(f))

  def filter[A](ls: List[A])(f: A => Boolean): List[A] =
    foldRight2(ls, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def filter2[A](ls: List[A])(f: A => Boolean): List[A] =
    flatMap(ls)(a => if (f(a)) List(a) else Nil)

  def addPairs(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairs(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)                                 => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _                                        => false
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t)                => hasSubsequence(t, sub)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Main extends App {

  // exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this is the matching case
    case Cons(h, t)                            => h + List.sum(t)
    case _                                     => 101
  }
  println(x)

  // exercise 3.2
  println(List.tail(List(1, 2, 3, 4)))

  // exercise 3.3
  println(List.setHead(1, List(2, 3, 4)))

  // exercise 3.4
  println(List.drop(List(1, 2, 3, 4), 2))

  // exercise 3.5
  println(List.dropWhile[Int](List(1, 2, 3, 4), _ < 3))

  // exercise 3.6
  println(List.init(List(1, 2, 3, 4, 5)))

  // exercise 3.7 Can product2 short circuit if a 0 is anywhere in the list?
  // The answer is it can't as implemented. It will always traverse the full list.
  println(List.product2(List(1, 0, 3, 4)))

  // exercise 3.8
  // we just get back the original list
  // Nil:List[Int] is just a type ascription (the book says type annotation but see: https://docs.scala-lang.org/style/types.html)
  println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

  // exercise 3.9
  println(List.length(List(1, 2, 3)))

  // exercise 3.10
  // as implemented, foldRight isn't tail recursive so will StackOverflow on something like:
  // println(List.foldRight(List((1 to 1000): _*), Nil:List[Int])(Cons(_, _)))
  val foldLeftTest = List(1, 3, 5)
  println(foldLeftTest)
  println(List.foldLeft(foldLeftTest, 0)(_ + _)) // == 9

  // exercise 3.11
  // write sum, product and length using foldLeft
  println(List.sumByFold(List(1, 2, 3)))     // 6
  println(List.productByFold(List(1, 2, 3))) // 6
  println(List.lengthByFold(List(1, 2, 3)))  // 3

  // exercise 3.12
  println(List.reverse(List(1, 2, 3))) // 3, 2, 1

  // exercise 3.13
  println(List.foldRight2(List((1 to 1000): _*), 0)(_ - _)) //-500
  println(List.foldLeft2(List((1 to 1000): _*), 0)(_ - _))  //-500500

  // exercise 3.14
  println(List.append2(List(1, 2, 3), List(4, 5, 6))) // 1, 2, 3, 4, 5, 6
  println(List.append3(List(1, 2, 3), List(4, 5, 6))) // 1, 2, 3, 4, 5, 6

  // exercise 3.15
  val listOfLists = List(
    List((1 to 10): _*),
    List((10 to 15): _*)
  )
  println(List.concat(listOfLists)) // 1,2,3,4,5,6

  // exercise 3.16
  println(List.addOne(List(1, 2, 3))) // 2,3,4

  // exercise 3.17
  val strings: List[String] = List.toStrings(List(1.0, 2.0, 3.0))
  println(strings) // "1","2","3"

  // exercise 3.18
  println(List.map(List('A', 'B', 'C'))(_.toInt)) // 65, 66, 67

  // exericse 3.19
  println(List.filter(List(1, 2, 3))(_ % 2 == 0))

  // exercise 3.20
  println(List.flatMap(List(1, 2, 3))(i => List(i, i))) // 1,1,2,2,3,3

  // exercise 3.21
  println(List.filter(List(1, 2, 3))(_ % 2 == 0))

  // exercise 3.22
  println(List.addPairs(List(1, 2, 3), List(4, 5, 6))) // List(5,7,9)

  // exercise 3.23
  println(List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))

  // exercise 3.24
  println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2))) // true

  // exercise 3.25
  // TODO: perhaps implement an apply method on leaf
  val tree: Tree[Int] = Branch(Leaf(1), Leaf(2))
  println(Tree.size(tree))  // 3 (1 branch, 2 Leaves)
  println(Tree.size2(tree)) // 3 (1 branch, 2 Leaves)

  // exercise 3.26
  println(Tree.maximum(tree))  // 2
  println(Tree.maximum2(tree)) // 2

  // exercise 3.27
  println(Tree.depth(tree))  // 1
  println(Tree.depth2(tree)) // 1

  // exercise 3.28
  println(Tree.map(tree)(_ * 2))  // 2, 4
  println(Tree.map2(tree)(_ * 2)) // 2, 4

  // exercise 3.29
  println(Tree.fold[Int, Tree[Int]](tree)(Leaf(_))(Branch(_, _))) // get back the same Tree

  // now let's concat every Leaf with newline in between
  println(Tree.fold(tree)(_.toString)(_ + "\n" + _))
}
