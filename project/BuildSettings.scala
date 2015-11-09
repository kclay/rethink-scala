import sbt._
import Keys._
import spray.boilerplate.BoilerplatePlugin.Boilerplate
import org.sbtidea.SbtIdeaPlugin._
import com.typesafe.sbt.SbtScalariform._
import sbtrelease._
import ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import ReleaseStateTransformations._

object BuildSettings {

    val buildSettings = Defaults.coreDefaultSettings ++ Boilerplate.settings ++ defaultScalariformSettings ++ Seq(
    organization := "com.rethinkscala",
    testOptions in Test := Seq(Tests.Filter(s ⇒ s.endsWith("Test"))),
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver.
        "-Ywarn-dead-code", // Warn when dead code is identified.
        "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
        "-Ywarn-nullary-override", // Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
        "-Ywarn-numeric-widen", // Warn when numerics are widened.
        "-target:jvm-1.8"
    ),
    scalaOrganization := "org.scala-lang",
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil)

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

    val repo = file("../kclay.github.io")
    val buildWithRelease = buildSettings ++ releaseSettings ++ Seq(
        releaseProcess := releaseSteps,
        publishArtifact in Test := false,
        publishTo <<= version {
            (v: String) ⇒ Some(Resolver.file("file", repo / (if (v.trim.endsWith("SNAPSHOT")) "snapshots" else "releases")))
        }
    )
}