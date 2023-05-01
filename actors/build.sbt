import Dependencies._

ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "app.vizion"

lazy val root = (project in file("."))
  .settings(
    name := "scalaQ-psi-collapse",
    libraryDependencies ++= Seq(
      Libraries.logback,
      Libraries.akka,
      Libraries.akkaTestkit
    )
  )