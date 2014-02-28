sealed trait Tree
case class CaseClass(fields: List[(String, Tree)]) extends Tree
case class Mapping(map: Map[String, Tree]) extends Tree
case class Sequence(seq: Iterable[Tree]) extends Tree
case class Leaf(value: Any) extends Tree





case class Scenario(agents: List[Agent], modes: Array[String], name: String, id: Long)
case class Agent(num: Int, times: Array[Double])

object Test {
  def main(args: Array[String]) {
    val a1 = Agent(1, Array(2.0, 5.0))
    val a2 = Agent(2, Array(3.0, 2.3))
    val s = Scenario(List(a1, a2), Array("baseline", "tolls"), "trial", 42)
  }
}
