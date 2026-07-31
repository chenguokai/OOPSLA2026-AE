ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "HT",
  )

libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.4.2"
libraryDependencies += "org.playframework" %% "play-json" % "3.0.0"

Compile / run / fork := true
Compile / run / javaOptions ++= Seq(
  "-Xms1G",
  "-Xmx4G"
)

// Test / sources := (Test / sources).value.filter(_.getName == "test101_vcd_phantom.scala")