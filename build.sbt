name := "scala-object-explorer"

version := "0.1"

scalaVersion := "2.10.3"

// src/main/scala is too verbose
scalaSource in Compile := baseDirectory.value

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

// Be able to collect all dependencies
packSettings
