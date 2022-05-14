val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "assembler",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta"          %% "munit"                    % "0.7.29" % Test,
    libraryDependencies += "org.typelevel"          %% "cats-effect"              % "3.2.5",
    libraryDependencies += "org.typelevel"          %% "cats-core"                % "2.7.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
   )
