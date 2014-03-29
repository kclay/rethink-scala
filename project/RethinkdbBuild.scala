
import sbt._
import Keys._
import spray.boilerplate.BoilerplatePlugin.Boilerplate
import org.sbtidea.SbtIdeaPlugin._

//import scalabuff.ScalaBuffPlugin._

import scalariform.formatter.preferences._

import com.typesafe.sbt.SbtScalariform._
import sbtrelease._
import ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import ReleaseStateTransformations._
import sbtprotobuf.{ProtobufPlugin => PB}


object BuildSettings {


  lazy val releaseSteps = Seq[ReleaseStep](
    checkSnapshotDependencies, // : ReleaseStep
    inquireVersions, // : ReleaseStep
    // runTest, // : ReleaseStep
    setReleaseVersion, // : ReleaseStep
    commitReleaseVersion, // : ReleaseStep, performs the initial git checks
    tagRelease, // : ReleaseStep
    publishArtifacts, // : ReleaseStep, checks whether `publishTo` is properly set up
    setNextVersion, // : ReleaseStep
    commitNextVersion, // : ReleaseStep
    pushChanges // : ReleaseStep, also checks that an upstream branch is properly configured
  )
  val buildSettings = Project.defaultSettings ++ Boilerplate.settings ++ defaultScalariformSettings ++ Seq(
    organization := "com.rethinkscala",
    testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
    version := "0.4.3-SNAPSHOT",
    scalaVersion := "2.11.0-RC3",

    scalacOptions ++= Seq(),

    scalaOrganization := "org.scala-lang",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil


  )

  val repo = file("../kclay.github.io")
  val buildWithRelease = buildSettings ++ releaseSettings ++ Seq(
    releaseProcess := releaseSteps,
    publishArtifact in Test := false,


    publishTo <<= version {
      (v: String) => Some(Resolver.file("file", repo / (if (v.trim.endsWith("SNAPSHOT")) "snapshots" else "releases")))


    }
  )
}

object RethinkdbBuild extends Build {

  import BuildSettings._

  val repos = Seq(
    "Local Maven Repository" at "file:///" + Path.userHome + "/.m2/repository",
    "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/",
    "Sonatype OSS Repository" at "https://oss.sonatype.org/content/groups/public/",
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  )

  def jackson = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % "2.2.2",
    "com.fasterxml.jackson.core" % "jackson-annotations" % "2.2.2",
    "com.fasterxml.jackson.core" % "jackson-databind" % "2.2.2",
    "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.3.1",

    "com.fasterxml.jackson.module" % "jackson-module-scala_2.11.0-RC3" % "2.4.0-SNAPSHOT"
  )


  def protobuf = {
    import PB._
    Option(file("bin/protoc.exe")).filter(_.exists()).map(f => Seq(
      protoc in protobufConfig := f.absolutePath
    )).getOrElse(Seq())
  }


  lazy val root = Project(
    "root",
    file("."),
    settings = buildWithRelease
  ) aggregate(lifted, core)
  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildWithRelease ++ PB.protobufSettings ++ Seq(

      javaSource in PB.protobufConfig <<= sourceManaged in Compile,
      // scalabuffVersion := scalaBuffVersion,
      resolvers ++= repos,
      //scalabuffArgs := Seq("--verbose", "--verbose"),


      //   scalabuffArgs := Seq("--stdout"),
      // set the directory for generated scala sources to src/main/generated_scala
      //generatedSource in scalaBuffConfig <<= (sourceDirectory in Compile)(_ / "generated_scala"),
      // generatedSource in protobufConfig <<= (sourceDirectory in Compile)(_ / "generated_java"),

      // it's not possible to generate both java and scala sources due to a "bug" in ScalaBuff.
      //addProtocCompatibility,
      libraryDependencies <++= (scalaVersion)(sv => Seq(
        "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
        //"com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
        "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11.0-RC3" % "2.0.0",
        "org.slf4j" % "slf4j-log4j12" % "1.7.6",

        "io.netty" % "netty" % "3.6.6.Final",
        "com.google.protobuf" % "protobuf-java" % "2.4.1",
        "joda-time" % "joda-time" % "2.3",
        "org.joda" % "joda-convert" % "1.5",
        "org.scala-lang" % "scala-reflect" % sv

        // "net.sandrogrzicic" %% "scalabuff-runtime" % scalaBuffVersion

      ) ++ jackson),


      ScalariformKeys.preferences := FormattingPreferences()
        .setPreference(AlignParameters, true)
        .setPreference(AlignSingleLineCaseStatements, true)
        .setPreference(DoubleIndentClassDeclaration, true)
        .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
        .setPreference(PreserveDanglingCloseParenthesis, true)
    ) ++ protobuf


  ) //.configs(ScalaBuff)

  lazy val lifted = Project(
    "lifted",
    file("lifted"),
    settings = buildWithRelease ++ Seq(
      // NOTE: macros are compiled with macro paradise 2.10
      // scalaVersion := "2.10.2-SNAPSHOT",
      //scalaOrganization := "org.scala-lang.macro-paradise",
      libraryDependencies <++= (scalaVersion)(sv => Seq(
        "org.scala-lang" % "scala-reflect" % sv,
        "org.scala-lang" % "scala-compiler" % sv,
        "org.scala-lang" % "scala-library" % sv
      )
      )
    )
  ) dependsOn (core)

}
