import sbt._
import sbt.Keys._
import scalabuff.ScalaBuffPlugin._

object RethinkdbBuild extends Build {

  lazy val rethinkdb = Project(
    id = "rethinkdb",
    base = file("."),
    settings = Project.defaultSettings  ++Seq(
      name := "rethinkdb",
      organization := "com.rethinkdb",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",
  
libraryDependencies ++= Seq(
			"org.scalatest" %% "scalatest" % "1.9.1" % "test",
			"net.sandrogrzicic" %% "scalabuff-runtime" % "1.1.1"
		)
    )
  ).configs(ScalaBuff)
}
