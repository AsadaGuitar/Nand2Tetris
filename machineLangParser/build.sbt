ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "parse.jar",
    name := "machineLangParser"
  )


resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  ,"Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)