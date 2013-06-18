resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"



resolvers += "gseitz@github" at "http://gseitz.github.com/maven/"


addSbtPlugin("com.github.sbt" %% "sbt-scalabuff" % "0.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.0.1")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.4.0")
