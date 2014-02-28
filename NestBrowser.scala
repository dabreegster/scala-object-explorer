import scala.reflect.runtime.{universe => ru}
import scala.collection.immutable

sealed trait Tree
case class CaseClass(fields: List[(String, Tree)]) extends Tree
case class Mapping(map: Map[String, Tree]) extends Tree
case class Sequence(seq: Iterable[Tree]) extends Tree
case class Leaf(value: Any) extends Tree





case class Scenario(agents: List[Agent], modes: Array[String], name: String, id: Long)
case class Agent(num: Int, times: Array[Double])

object Test {
  // from
  // http://stackoverflow.com/questions/16079113/scala-2-10-reflection-how-do-i-extract-the-field-values-from-a-case-class
  def caseClassParamsOf[T: ru.TypeTag]: immutable.ListMap[String, ru.Type] = {
    val tpe = ru.typeOf[T]
    val constructorSymbol = tpe.declaration(ru.nme.CONSTRUCTOR)
    val defaultConstructor =
      if (constructorSymbol.isMethod) constructorSymbol.asMethod
      else {
        val ctors = constructorSymbol.asTerm.alternatives
        ctors.map { _.asMethod }.find { _.isPrimaryConstructor }.get
      }

    return immutable.ListMap[String, ru.Type]() ++ defaultConstructor.paramss.reduceLeft(_ ++ _).map {
      sym => sym.name.toString -> tpe.member(sym.name).asMethod.returnType
    }
  }

  def main(args: Array[String]) {
    val a1 = Agent(1, Array(2.0, 5.0))
    val a2 = Agent(2, Array(3.0, 2.3))
    val s = Scenario(List(a1, a2), Array("baseline", "tolls"), "trial", 42)
    println(caseClassParamsOf[Scenario])
  }
}
