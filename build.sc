import mill._, scalalib._

object cafes extends ScalaModule{
  def scalaVersion = "2.12.4"
}

object chapter2 extends ScalaModule {
  def scalaVersion = "2.12.4"
  def ivyDeps = Agg(
    ivy"org.scalaz::scalaz-core::7.2.27"
  )
}
