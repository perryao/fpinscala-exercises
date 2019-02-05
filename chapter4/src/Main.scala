package chapter4
import util.Try

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // => Option[B] is a call-by-name parameter: https://docs.scala-lang.org/tour/by-name-parameters.html
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // => Option[B] is a call-by-name parameter: https://docs.scala-lang.org/tour/by-name-parameters.html
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None) 
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum /xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Main extends App {
  // exercise 4.1
  val some = Some("value")
  val none: Option[String] = None
  // map
  println(some.map(_.toUpperCase)) // Some("VALUE")
  println(none.map(_.toUpperCase)) // None

  // flatMap
  println(some.flatMap(a => Some(a.toUpperCase))) // Some("VALUE")
  println(none.flatMap(a => Some(a.toUpperCase))) // None

  // getOrElse
  println(some.getOrElse("anotherValue")) // value
  println(none.getOrElse("anotherValue")) // anotherValue
  
  // orElse
  println(s"orElse: ${some.orElse(Some("anotherValue"))}") // Some("value")
  println(s"orElse: ${none.orElse(Some("anotherValue"))}") // Some("anotherValue")


  // filter
  println(some.filter(_ => true)) // Some("value")
  println(some.filter(_ => false)) // None

  // exercise 4.2
  println(Option.variance(Seq(1,2,3)))

  // exercise 4.3
  println(Option.map2(Some(2), Some(2))(_ + _)) //4
  println(Option.map2(Some(2), None:Option[Int])(_ + _)) //None

  // exercise 4.4
  println(Option.sequence(List(Some(1), Some(2)))) // Some(List(1,2))

  // exercise 4.5
  println(Option.traverse[String, Int](List("1", "2", "3"))(a => Some(a.toInt))) // Some(List(1,2,3))
  println(Option.traverse[String, Int](List("1", "2", "3"))(a => None)) // None

  println(Option.sequenceViaTraverse(List(Some(1), Some(2)))) // Some(List(1,2))
}
