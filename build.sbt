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
  "Maven central http" at "http://repo1.maven.org/maven2"
)

libraryDependencies ++= Seq(
  "org.scalatest" %%  "scalatest"   % "2.2.1" % "test",
  "org.mockito" % "mockito-core" % "1.10.19" % "test"
)
