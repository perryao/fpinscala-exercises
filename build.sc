import $ivy.`com.lihaoyi::mill-contrib-bloop:0.4.0-2-4dbbce`
import mill._, scalalib._, scalafmt._

trait BaseModule extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.12.8"
  object test extends Tests { 
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
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

object chapter6 extends BaseModule {}
