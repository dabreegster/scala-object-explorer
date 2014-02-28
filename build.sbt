name := "nestbrowser"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

// src/main/scala is too verbose
scalaSource in Compile := baseDirectory.value

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

// Be able to collect all dependencies
packSettings
