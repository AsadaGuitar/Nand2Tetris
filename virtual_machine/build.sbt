ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "virtual_machine"
  )

libraryDependencies ++= {
  val catsVersion = "2.3.0"

  Seq(
    "org.typelevel"    %%    "cats-core"        %    "2.7.0",
    "org.typelevel"    %%    "cats-effect"      %    "3.3.11",
    "org.typelevel"    %%    "log4cats-core"    %    catsVersion,
    "org.typelevel"    %%    "log4cats-slf4j"   %    catsVersion,
    "ch.qos.logback"   %     "logback-classic"  %    "1.2.11"      %  Runtime
  )
}
