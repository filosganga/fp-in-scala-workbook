
organization := "com.filippodeluca"

name := "fp-in-scala"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq (
  "org.typelevel" %% "cats" % "0.6.0",
  "org.scalatest" %% "scalatest" % "2.2.4" % Test,
  "org.scalacheck" %% "scalacheck" % "1.12.2" % Test
)