import $ivy.`ch.epfl.scala::mill-bloop:1.2.5`
import mill._, scalalib._, scalafmt._

trait BaseModule extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.12.8"
}

object cafes extends BaseModule {}

object chapter2 extends BaseModule {
  def ivyDeps = Agg(
    ivy"org.scalaz::scalaz-core::7.2.27"
  )
}

object chapter3 extends BaseModule {}

object chapter4 extends BaseModule {}

object chapter5 extends BaseModule {}
