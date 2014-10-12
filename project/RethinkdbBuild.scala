
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
    version := "0.4.5",
    scalaVersion := "2.11.1",

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

  val jacksonVersion = "2.4.0-rc2"
  val jacksonScalaVersion = "2.4.0-rc2"
  def jackson = Seq(
   "com.fasterxml.jackson.core" % "jackson-core" % jacksonVersion,
   "com.fasterxml.jackson.core" % "jackson-annotations" % jacksonVersion,
    "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
    "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % jacksonVersion,

    "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonScalaVersion
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
  ) aggregate( core,changeFeed)
  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildWithRelease ++ PB.protobufSettings ++ Seq(

      javaSource in PB.protobufConfig <<= sourceManaged in Compile,
      // scalabuffVersion := scalaBuffVersion,
      resolvers ++= repos,

      libraryDependencies <++= (scalaVersion)(sv => Seq(

        "org.scalatest" %% "scalatest" % "2.1.3" % "test",
        //"com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
        "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.0.3",
        "org.slf4j" % "slf4j-log4j12" % "1.7.6",

        "io.netty" % "netty" % "3.6.6.Final",
        "com.google.protobuf" % "protobuf-java" % "2.5.0",
        "joda-time" % "joda-time" % "2.3",
        "org.joda" % "joda-convert" % "1.5",
        "org.scala-lang" % "scala-reflect" % sv,
        "org.scala-lang.modules" %% "scala-xml" % "1.0.1"  % "test"
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


  lazy val changeFeed = Project("change-feed",
  file("change-feed"),
    settings = buildWithRelease).dependsOn(core).aggregate(core)
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
