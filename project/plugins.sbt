resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers+="sbt-plugin-snapshots" at "http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"


resolvers += "gseitz@github" at "http://gseitz.github.com/maven/"


addSbtPlugin("com.github.sbt" %% "sbt-scalabuff" % "0.2")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.3.0-SNAPSHOT")
