resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"



resolvers += "gseitz@github" at "http://gseitz.github.com/maven/"

resolvers += Resolver.url(
  "sbt-plugin-releases",
  new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.7")

//addSbtPlugin("com.github.sbt" %% "sbt-scalabuff" % "0.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.0.1")

addSbtPlugin("com.github.gseitz" % "sbt-protobuf" % "0.3.0")


addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.1")
