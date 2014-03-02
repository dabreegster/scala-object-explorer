name := "scala-object-explorer"

version := "0.1"

scalaVersion := "2.10.3"

// src/main/scala is too verbose
scalaSource in Compile := baseDirectory.value

autoCompilerPlugins := true

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full,
  compilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)
)

initialCommands in console := """
  import scala.util.dump._
  case class Scenario(agents: List[Agent], modes: Array[String], name: String, id: Long)
  case class Agent(num: Int, times: Map[String, Double])
  val a1 = Agent(1, Map("baseline" -> 2.0, "tolls" -> 5.0))
  val a2 = Agent(2, Map("baseline" -> 3.0, "tolls" -> 2.3))
  val s = Scenario(List(a1, a2), Array("baseline", "tolls"), "trial", 42)
"""

// Be able to collect all dependencies
packSettings
