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
  def caseClassParamsOf(tpe: ru.Type): immutable.ListMap[String, ru.Type] = {
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

  def get_field(obj: Any, obj_tpe: ru.Type, field: String): Any = {
    val m = ru.runtimeMirror(obj.getClass.getClassLoader)
    val term = obj_tpe.declaration(ru.newTermName(field)).asTerm
    val im = m.reflect(obj)
    return im.reflectField(term).get
  }

  def dump(obj: Any, obj_type: ru.Type, level: Int) {
    obj match {
      case ls: List[Any] =>
        // get parameter type from
        // http://stackoverflow.com/questions/12842729/finding-type-parameters-via-reflection-in-scala-2-10
        ls.foreach(x => dump(x, obj_type.asInstanceOf[ru.TypeRefApi].args.head, level + 1))
      case ls: Array[_] =>
        ls.foreach(x => dump(x, obj_type.asInstanceOf[ru.TypeRefApi].args.head, level + 1))
      // breaks on caseClassParamsOf
      case x: String => println(("  " * level) + obj)
      case _ => {
        // TODO messy. but seems to work.
        val fields = caseClassParamsOf(obj_type)
        if (fields.isEmpty) {
          // it's a primitive/leaf
          println(("  " * level) + obj)
        } else {
          for ((name, tpe) <- fields) {
            val value = get_field(obj, obj_type, name)
            println(("  " * level) + name + " = ")
            dump(value, tpe, level + 1)
          }
        }
      }
    }
  }

  def main(args: Array[String]) {
    val a1 = Agent(1, Array(2.0, 5.0))
    val a2 = Agent(2, Array(3.0, 2.3))
    val s = Scenario(List(a1, a2), Array("baseline", "tolls"), "trial", 42)
    dump(s, ru.typeOf[Scenario], 0)
  }
}
