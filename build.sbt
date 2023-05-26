ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "fr"
  )

libraryDependencies += "org.graphstream" % "gs-core" % "2.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.graphstream" % "gs-ui-swing" % "2.0"
libraryDependencies += "org.graphstream" % "gs-ui-javafx" % "2.0"



libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"

