package chapter4

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
}
