name := "dungeoner"

version := "1.0"

scalaVersion := "2.12.4"

val janusVersion = "0.2.0"

libraryDependencies ++= Seq (
  "com.michaelpollmeier" %% "gremlin-scala" % "3.3.0.5",
  "org.janusgraph" % "janusgraph-core" % janusVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3" % Test,
  "org.scalatest" %% "scalatest" % "3.0.3" % Test
)

fork in Test := true

resolvers ++= Seq(
  Resolver.mavenLocal,
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public"
)
