
import sbt._
import Keys._

//import scalabuff.ScalaBuffPlugin._

import scalariform.formatter.preferences._

import com.typesafe.sbt.SbtScalariform._
import sbtrelease._
import ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import ReleaseStateTransformations._


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
  val buildSettings = Project.defaultSettings ++ defaultScalariformSettings ++ Seq(
    organization := "com.rethinkscala",
    testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
    version := "0.4.2",
    scalaVersion := "2.10.2",

    scalacOptions ++= Seq(),

    scalaOrganization := "org.scala-lang",
    resolvers += Resolver.sonatypeRepo("snapshots")


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
    "Sonatype OSS Repository" at "https://oss.sonatype.org/content/groups/public/"
    //"Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  )

  def jackson(v: String) = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % v,
    "com.fasterxml.jackson.core" % "jackson-annotations" % v,
    "com.fasterxml.jackson.core" % "jackson-databind" % v,
    //"com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % ("[2.2,"+v+"]") % "test",

    "com.fasterxml.jackson.module" %% "jackson-module-scala" % v
  )



  lazy val root = Project(
    "root",
    file("."),
    settings = buildWithRelease
  ) aggregate(lifted, core)
  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildWithRelease ++ Seq(


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
        "org.scalatest" %% "scalatest" % "1.9.1" % "test",

        "io.netty" % "netty" % "3.6.6.Final",
        "com.google.protobuf" % "protobuf-java" % "2.4.1",
        "org.scala-lang" % "scala-reflect" % sv

        // "net.sandrogrzicic" %% "scalabuff-runtime" % scalaBuffVersion

      ) ++ jackson("2.2.2")),


      ScalariformKeys.preferences := FormattingPreferences()
        .setPreference(AlignParameters, true)
        .setPreference(AlignSingleLineCaseStatements, true)
        .setPreference(DoubleIndentClassDeclaration, true)
        .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
        .setPreference(PreserveDanglingCloseParenthesis, true)
    )


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
