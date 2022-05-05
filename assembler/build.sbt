ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "assembler_scala"
  )

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.11"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"