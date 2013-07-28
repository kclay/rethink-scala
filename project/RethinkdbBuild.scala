
import sbt._
import Keys._
import scalabuff.ScalaBuffPlugin._
import scalariform.formatter.preferences._

import com.typesafe.sbt.SbtScalariform._
import sbtrelease._
import ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import ReleaseStateTransformations._

object BuildSettings {


  object GitWindows extends Vcs with GitLike {
    val commandName = "git"
    private lazy val exec = executableName(commandName)

    override def cmd(args: Any*): ProcessBuilder = Process(exec +: args.map(_.toString), cwd = None, extraEnv = ("Path", System.getenv("Path")))

    override protected def executableName(command: String) = {
      val maybeOsName = sys.props.get("os.name").map(_.toLowerCase)
      val maybeIsWindows = maybeOsName.filter(_.contains("windows"))
      maybeIsWindows.map(_ => command).getOrElse(command)
    }

    protected val markerDirectory = ".git"

    private lazy val trackingBranchCmd = cmd("config", "branch.%s.merge" format currentBranch)

    private def trackingBranch: String = (trackingBranchCmd !!).trim.stripPrefix("refs/heads/")

    private lazy val trackingRemoteCmd: ProcessBuilder = cmd("config", "branch.%s.remote" format currentBranch)

    def trackingRemote: String = (trackingRemoteCmd !!) trim

    def hasUpstream = trackingRemoteCmd ! devnull == 0 && trackingBranchCmd ! devnull == 0

    def currentBranch = (cmd("symbolic-ref", "HEAD") !!).trim.stripPrefix("refs/heads/")

    def currentHash = revParse(currentBranch)

    private def revParse(name: String) = (cmd("rev-parse", name) !!) trim

    def isBehindRemote = (cmd("rev-list", "%s..%s/%s".format(currentBranch, trackingRemote, trackingBranch)) !! devnull).trim.nonEmpty

    def tag(name: String, comment: String, force: Boolean = false) = cmd("tag", "-a", name, "-m", comment, if (force) "-f" else "")

    def existsTag(name: String) = cmd("show-ref", "--quiet", "--tags", "--verify", "refs/tags/" + name) ! devnull == 0

    def checkRemote(remote: String) = fetch(remote)

    def fetch(remote: String) = cmd("fetch", remote)

    def status = cmd("status", "--porcelain")

    def pushChanges = pushCurrentBranch #&& pushTags

    private def pushCurrentBranch = {
      val localBranch = currentBranch
      cmd("push", trackingRemote, "%s:%s" format(localBranch, trackingBranch))
    }

    private def pushTags = cmd("push", "--tags", trackingRemote)
  }


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
    version := "0.3-SNAPSHOT",
    scalaVersion := "2.10.2",

    scalacOptions ++= Seq(),

    scalaOrganization := "org.scala-lang",
    resolvers += Resolver.sonatypeRepo("snapshots")


  )

  val repo = file("../kclay.github.io")
  val buildWithRelease = buildSettings ++ releaseSettings ++ Seq(
    releaseProcess := releaseSteps,
    publishArtifact in Test := false,
    versionControlSystem := Some(GitWindows),

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

  val scalaBuffVersion = "1.3.1"

  lazy val root = Project(
    "root",
    file("."),
    settings = buildWithRelease
  ) aggregate(lifted, core)
  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildWithRelease ++ scalabuffSettings ++ Seq(


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
