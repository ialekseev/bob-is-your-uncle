name := "bob-is-your-uncle"
organization := "com.ialekseev"
version := "0.1.0"
scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:implicitConversions",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-target:jvm-1.7",
  "-encoding", "UTF-8"
)

resolvers ++= Seq(
  "Maven central http" at "http://repo1.maven.org/maven2",
  "BFil Nexus Releases" at "http://nexus.b-fil.com/nexus/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http-experimental" % "2.4.9",
  "de.heikoseeberger" %% "akka-http-json4s" % "1.9.0",

  "org.scala-lang" % "scala-compiler" % scalaVersion.value,

  "org.scalaz" %% "scalaz-core" % "7.2.7",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.7",
  "org.scalaz" %% "scalaz-effect" % "7.2.7",
  "co.fs2" %% "fs2-core" % "0.9.4",
  "co.fs2" %% "fs2-io" % "0.9.4",
  "co.fs2" %% "fs2-scalaz" % "0.2.0",
  "io.verizon.delorean" %% "core" % "1.2.41-scalaz-7.1",

  "com.bfil" %% "automapper" % "0.4.0",

  "org.json4s" %% "json4s-native" % "3.4.0",
  "org.json4s" %% "json4s-ext" % "3.4.0",
  "org.scalaj" %% "scalaj-http" % "2.2.0",

  "com.github.scopt" %% "scopt" % "3.5.0",

  "org.scalatest" %%  "scalatest"   % "2.2.1" % "test",
  "org.mockito" % "mockito-core" % "1.10.19" % "test",
  "com.typesafe.akka" % "akka-http-testkit_2.11" % "2.4.9" % "test"
)

reForkOptions ~= (_.copy(connectInput = true))
