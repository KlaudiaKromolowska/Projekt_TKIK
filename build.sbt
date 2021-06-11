ThisBuild / scalaVersion := "2.12.10"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "org.lamac"
ThisBuild / organizationName := "lamac"

lazy val root = (project in file("."))
  .settings(
    name := "lamac",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    scalacOptions ++= Seq("-deprecation", "-feature")
  )
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
