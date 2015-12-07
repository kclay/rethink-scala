import sbt._
import Keys._
import sbtrelease.ReleasePlugin
import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform._
import sbtprotobuf.{ProtobufPlugin ⇒ PB}

import BuildSettings._


object RethinkdbBuild extends Build {

  val protobuf = {
    import PB._
    Option(file("bin/protoc.exe")).filter(_.exists()).map(f ⇒ Seq(
      protoc in protobufConfig := f.absolutePath)).getOrElse(Seq())
  }

  val repos = Seq(
    "Local Maven Repository" at "file:///" + Path.userHome + "/.m2/repository",
    "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/",
    "Sonatype OSS Repository" at "https://oss.sonatype.org/content/groups/public/",
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"
  )

  object Versions {
    val jacksonVersion = "2.5.3"
    val jacksonScalaVersion = "2.5.2"
  }


  lazy val root = Project(
    "root",
    file("."),
    settings = buildWithRelease) aggregate (core)
  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildWithRelease ++ PB.protobufSettings ++ Seq(

      javaSource in PB.protobufConfig <<= sourceManaged in Compile,
      resolvers ++= repos,

      libraryDependencies <++= (scalaVersion) (sv ⇒ Seq(
        "org.scalatest" %% "scalatest" % "2.1.3" % "test",
        "junit" % "junit" % "4.8.1" % Test,
        "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.0.3",
        "org.slf4j" % "slf4j-log4j12" % "1.7.6",
        "io.netty" % "netty-all" % "4.0.27.Final",
        "joda-time" % "joda-time" % "2.3",
        "org.joda" % "joda-convert" % "1.5",
        "org.scala-lang" % "scala-reflect" % sv,
        "com.google.guava" % "guava" % "18.0",
        "com.googlecode.thread-weaver" % "threadweaver" % "0.2" % "test",
        "org.scalaz.stream" %% "scalaz-stream" % "0.7.1",
        "org.scala-lang.modules" %% "scala-xml" % "1.0.1" % "test",
        "com.fasterxml.jackson.core" % "jackson-core" % Versions.jacksonVersion,
        "com.fasterxml.jackson.core" % "jackson-annotations" % Versions.jacksonVersion,
        "com.fasterxml.jackson.core" % "jackson-databind" % Versions.jacksonVersion,
        "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % Versions.jacksonVersion,
        "com.fasterxml.jackson.module" %% "jackson-module-scala" % Versions.jacksonScalaVersion
      )),

      fork in Test := true,
      parallelExecution in Test := false,

      ScalariformKeys.preferences := FormattingPreferences()
        .setPreference(RewriteArrowSymbols, true)
        .setPreference(AlignParameters, true)
        .setPreference(AlignSingleLineCaseStatements, true)) ++ protobuf)
    .enablePlugins(ReleasePlugin) //.configs(ScalaBuff)

}
