package chapter4
import util.Try

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  // => Option[B] is a call-by-name parameter: https://docs.scala-lang.org/tour/by-name-parameters.html
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  // => Option[B] is a call-by-name parameter: https://docs.scala-lang.org/tour/by-name-parameters.html
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A]          = flatMap(a => if (f(a)) Some(a) else None)
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}

case class Some[+A](get: A) extends Option[A]
case object None            extends Option[Nothing]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))
  }
  // Should we implement getOrElse like in Option?
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => f(a)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(a) => Right(a)
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a  <- this
      b1 <- b
    } yield f(a, b1)
}

case class Left[+E](value: E)  extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def saveDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))((a, b) => a.map2(b)(_ :: _))

  def sequenceViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((h, t) => f(h).map2(t)(_ :: _))
}

object Main extends App {
  // exercise 4.1
  val some                 = Some("value")
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
  println(some.filter(_ => true))  // Some("value")
  println(some.filter(_ => false)) // None

  // exercise 4.2
  println(Option.variance(Seq(1, 2, 3)))

  // exercise 4.3
  println(Option.map2(Some(2), Some(2))(_ + _))           //4
  println(Option.map2(Some(2), None: Option[Int])(_ + _)) //None

  // exercise 4.4
  println(Option.sequence(List(Some(1), Some(2)))) // Some(List(1,2))

  // exercise 4.5
  println(Option.traverse[String, Int](List("1", "2", "3"))(a => Some(a.toInt))) // Some(List(1,2,3))
  println(Option.traverse[String, Int](List("1", "2", "3"))(a => None))          // None

  println(Option.sequenceViaTraverse(List(Some(1), Some(2)))) // Some(List(1,2))

  // exercise 4.6
  val l: Either[String, Int] = Left("This is an error")
  val r: Either[String, Int] = Right(2)

  // map
  println(l.map(_ + 2)) // Left("This is an error")
  println(r.map(_ + 2)) // Right(4)

  // flatMap
  // return original Either, a Left
  println(l.flatMap(Left(_))) // Left("This is an error")
  // convert Left to Right
  println(l.flatMap(_ => Right(5))) // Right(5)
  // convert a Right to a Left
  println(r.flatMap(a => Left(s"didn't want $a"))) // Left("didn't want 2")

  // orElse
  println(l.orElse(Left("some error"))) // Left("some error")
  println(r.orElse(Left("some error"))) // Right(2)

  //map2
  println(l.map2(Right(6))(_ + _)) // Left("this is an error")
  println(r.map2(Right(6))(_ + _)) // 8

  // exercise 4.7
  println(Either.sequence(List(Right(1), Right(2), Right(3))))                            // Right(List(1,2,3))
  println(Either.sequenceViaTraverse(List(Right(1), Right(2), Right(3))))                 // Right(List(1,2,3))
  println(Either.sequence(List(Right(1), Left("this is an error"), Right(3))))            // Left("this is an error")
  println(Either.sequenceViaTraverse(List(Right(1), Left("this is an error"), Right(3)))) // Left("this is an error")

  println(
    Either.traverse[String, Int, Int](List(1, 2, 3, 4, 5))(
      x =>
        if (x < 6) Right(x)
        else Left("too low")
    )
  ) // Right(List(1,2,3,4,5))
  println(
    Either.traverse[String, Int, Int](List(1, 2, 3, 4, 5))(
      x =>
        if (x > 2) Right(x)
        else Left(s"$x is too low")
    )
  ) // Left("1 is too low")
}
