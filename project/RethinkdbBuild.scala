
import sbt._
import Keys._


object RethinkdbBuild extends Build {


  val scalaBuffVersion = "1.2.0-SNAPSHOT"
  lazy val rethinkdb = Project(
    id = "rethinkdb",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "rethinkdb",
      organization := "com.rethinkdb",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",

      //   scalabuffArgs := Seq("--stdout"),
      // set the directory for generated scala sources to src/main/generated_scala
      //generatedSource in scalaBuffConfig <<= (sourceDirectory in Compile)(_ / "generated_scala"),
      // generatedSource in protobufConfig <<= (sourceDirectory in Compile)(_ / "generated_java"),

      // it's not possible to generate both java and scala sources due to a "bug" in ScalaBuff.
      //addProtocCompatibility,
      libraryDependencies <++=(scalaVersion)(sv=> Seq(
        "org.scalatest" %% "scalatest" % "1.9.1" % "test",
        "com.google.protobuf" % "protobuf-java" % "2.5.0",
        "io.netty" % "netty"%"3.6.3.Final",
        "commons-pool"%"commons-pool" %"1.6",
        "org.scala-lang" % "scala-reflect" % sv
        // "net.sandrogrzicic" %% "scalabuff-runtime" % scalaBuffVersion
      ))
    )
  )//.configs(ScalaBuff)

  lazy val connectionPool =uri("git://github.com/jamesgolick/scala-connection-pool.git")
}
