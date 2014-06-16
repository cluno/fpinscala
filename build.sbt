organization := "luno"

name := "fpinscala"

scalaVersion := "2.10.3"

resolvers += "Typesafe Repository" at "Http://repo.typesafe.com/typesafe/release"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.4" % "test",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)
