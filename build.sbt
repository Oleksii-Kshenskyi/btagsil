import Dependencies._

ThisBuild / scalaVersion     := "3.2.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.einax.btagsil"
ThisBuild / organizationName := "einax"

val jarName = "btagsil.jar"
assembly/assemblyJarName := jarName

lazy val root = (project in file("."))
  .settings(
    name := "btagsil",
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
