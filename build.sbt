ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "protobuf-scala3",
    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java" % "3.24.2",
      "com.google.protobuf" % "protobuf-java-util" % "3.24.2",

      // scalatest
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
    )
  )
