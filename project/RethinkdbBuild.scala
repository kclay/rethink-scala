
import sbt._
import Keys._
import scalabuff.ScalaBuffPlugin._
import com.typesafe.sbt.SbtScalariform.scalariformSettings
import scalariform.formatter.preferences._

import com.typesafe.sbt.SbtScalariform._


object RethinkdbBuild extends Build {


  val repos = Seq(
    "Local Maven Repository" at "file:///" + Path.userHome + "/.m2/repository",
    "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"
    //"Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  )

  def jackson(v: String) = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % v,
    "com.fasterxml.jackson.core" % "jackson-annotations" % v,
    "com.fasterxml.jackson.core" % "jackson-databind" % v,
    //"com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % ("[2.2,"+v+"]") % "test",

    "com.fasterxml.jackson.module" %% "jackson-module-scala" % v
  )

  val scalaBuffVersion = "1.3.1"
  lazy val root = Project(
    id = "rethink-scala",
    base = file("."),
    settings = Project.defaultSettings ++ scalabuffSettings ++ scalariformSettings ++ Seq(
      name := "rethink-scala",
      organization := "com.rethinkscala",
      testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",
      scalabuffVersion := scalaBuffVersion,
      resolvers ++= repos,


      //   scalabuffArgs := Seq("--stdout"),
      // set the directory for generated scala sources to src/main/generated_scala
      //generatedSource in scalaBuffConfig <<= (sourceDirectory in Compile)(_ / "generated_scala"),
      // generatedSource in protobufConfig <<= (sourceDirectory in Compile)(_ / "generated_java"),

      // it's not possible to generate both java and scala sources due to a "bug" in ScalaBuff.
      //addProtocCompatibility,
      libraryDependencies <++= (scalaVersion)(sv => Seq(
        "org.scalatest" %% "scalatest" % "1.9.1" % "test",

        "io.netty" % "netty" % "3.6.3.Final",
        "org.scala-lang" % "scala-reflect" % sv,
        "net.sandrogrzicic" %% "scalabuff-runtime" % scalaBuffVersion

      ) ++ jackson("2.2.2")),


      ScalariformKeys.preferences := FormattingPreferences()
        .setPreference(AlignParameters, true)
        .setPreference(AlignSingleLineCaseStatements, true)
        .setPreference(DoubleIndentClassDeclaration, true)
        .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
        .setPreference(PreserveDanglingCloseParenthesis, true)
    )
  ).configs(ScalaBuff)

}
