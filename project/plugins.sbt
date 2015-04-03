resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"



resolvers += "gseitz@github" at "http://gseitz.github.com/maven/"

resolvers += Resolver.url(
  "sbt-plugin-releases",
  new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/")
)(Resolver.ivyStylePatterns)

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.4")

//addSbtPlugin("com.github.sbt" %% "sbt-scalabuff" % "0.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("com.github.gseitz" % "sbt-protobuf" % "0.4.0")


addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.7.0-SNAPSHOT")

addSbtPlugin("io.spray" % "sbt-boilerplate" % "0.5.1")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.6.0")


