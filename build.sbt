ThisBuild / scalaVersion := "3.0.0"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.alvo"
ThisBuild / organizationName := "alvo"

lazy val root = project
  .in(file("."))
  .settings(
    name := "stack-vm",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "org.typelevel" %% "cats-core" % "2.6.1"
    )
  )

addCommandAlias("cd", "project")
addCommandAlias("ls", "projects")
addCommandAlias("c", "compile")
addCommandAlias("rel", "reload")
addCommandAlias("r", "run")
