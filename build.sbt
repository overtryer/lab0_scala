ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.8"

lazy val root = (project in file("."))
  .settings(
    name := "lab0_scala",
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "ujson" % "4.0.2",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
