name := "dungeoner"

version := "1.0"

scalaVersion := "2.11.8"

val titanVersion = "1.0.0"

libraryDependencies ++= Seq (
  "com.michaelpollmeier" %% "gremlin-scala" % "3.0.2-incubating.2",
  "com.thinkaurelius.titan" % "titan-core" % titanVersion,
  "org.scalatest" %% "scalatest" % "2.2.5" % "test"
)

resolvers += Resolver.mavenLocal

resolvers += "Sonatype repo" at "https://oss.sonatype.org/content/repositories/snapshots"
