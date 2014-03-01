scala-object-explorer
=====================

Say you have nested case classes that you'd like to examine. One convenient method is to just dump
them in a tree form.

Basic usage:

```scala
import scala.util.{DumpTree, AsciiTreeViewer}

// Example case class
case class Scenario(agents: List[Agent], modes: Array[String], name: String, id: Long)
case class Agent(num: Int, times: Map[String, Double])

val a1 = Agent(1, Map("baseline" -> 2.0, "tolls" -> 5.0))
val a2 = Agent(2, Map("baseline" -> 3.0, "tolls" -> 2.3))
val s = Scenario(List(a1, a2), Array("baseline", "tolls"), "trial", 42)
// You have to hardcode the type name to preserve a type tag at compile-time before erasure
AsciiTreeViewer.view(DumpTree.build(s, scala.reflect.runtime.universe.typeOf[Scenario]))
```

The output is:

```
Scenario
  agents = [
    Agent
      num = 1
      times = {
        baseline -> 2.0
        tolls -> 5.0
      }
    Agent
      num = 2
      times = {
        baseline -> 3.0
        tolls -> 2.3
      }
  ]
  modes = [
    baseline
    tolls
  ]
  name = trial
  id = 42
```

It wouldn't be hard to make a graphical viewer too using Swing's JTree.
