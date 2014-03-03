import scala.util.dump._

// Example case class
case class Scenario(agents: List[Agent], modes: Array[String], name: String, id: Long)
case class Agent(num: Int, times: Map[String, Double])

object Test {
  def main(args: Array[String]) {
    // Some data
    val a1 = Agent(1, Map("baseline" -> 2.0, "tolls" -> 5.0))
    val a2 = Agent(2, Map("baseline" -> 3.0, "tolls" -> 2.3))
    val s = Scenario(List(a1, a2), Array("baseline", "tolls"), "trial", 42)

    // Dump!
    println(AsciiTreeView(s))
  }
}
