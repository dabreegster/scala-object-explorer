import scala.reflect.runtime.{universe => ru}
import scala.collection.immutable

sealed trait Tree
case class Object(fields: immutable.ListMap[String, Tree]) extends Tree
case class Mapping(map: Map[String, Tree]) extends Tree
case class Sequence(seq: List[Tree]) extends Tree
case class Leaf(value: Any) extends Tree

object Tree {
  def build(obj: Any, obj_type: ru.Type): Tree = {
    obj match {
      case ls: List[Any] => Sequence(ls.map(
        // get parameter type from
        // http://stackoverflow.com/questions/12842729/finding-type-parameters-via-reflection-in-scala-2-10
        x => build(x, obj_type.asInstanceOf[ru.TypeRefApi].args.head)
      ).toList)
      case ls: Array[_] => Sequence(ls.map(
        x => build(x, obj_type.asInstanceOf[ru.TypeRefApi].args.head)
      ).toList)
      // breaks on caseClassParamsOf, so a special case
      case x: String => Leaf(x)
      case _ => {
        // TODO messy. but seems to work.
        val fields = caseClassParamsOf(obj_type)
        if (fields.isEmpty) {
          Leaf(obj)
        } else {
          Object(immutable.ListMap[String, Tree]() ++ (fields.keys.map(name => name -> build(get_field(obj, obj_type, name), fields(name)))))
        }
      }
    }
  }

  // from
  // http://stackoverflow.com/questions/16079113/scala-2-10-reflection-how-do-i-extract-the-field-values-from-a-case-class
  private def caseClassParamsOf(tpe: ru.Type): immutable.ListMap[String, ru.Type] = {
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

  private def get_field(obj: Any, obj_tpe: ru.Type, field: String): Any = {
    val m = ru.runtimeMirror(obj.getClass.getClassLoader)
    val term = obj_tpe.declaration(ru.newTermName(field)).asTerm
    val im = m.reflect(obj)
    return im.reflectField(term).get
  }
}

case class Scenario(agents: List[Agent], modes: Array[String], name: String, id: Long)
case class Agent(num: Int, times: Array[Double])

object Test {
  def main(args: Array[String]) {
    val a1 = Agent(1, Array(2.0, 5.0))
    val a2 = Agent(2, Array(3.0, 2.3))
    val s = Scenario(List(a1, a2), Array("baseline", "tolls"), "trial", 42)
    val t = Tree.build(s, ru.typeOf[Scenario])
    println(s)
    println(t)
  }
}
