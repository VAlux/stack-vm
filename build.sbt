ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.alvo"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "stack-vm",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.typelevel" %% "cats-core" % "1.5.0")
  )

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
  "-Ypartial-unification")

logBuffered in Test := false
