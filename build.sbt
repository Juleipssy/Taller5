ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Taller5"
  )

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.21"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.21" % "test"
