package scala.util

import scala.reflect.runtime.{universe => ru}
import scala.collection.immutable

sealed trait DumpTree
case class Object(name: String, fields: immutable.ListMap[String, DumpTree]) extends DumpTree
// Maps with non-string keys will be coerced
case class Mapping(map: Map[String, DumpTree]) extends DumpTree
case class Sequence(seq: List[DumpTree]) extends DumpTree
case class Leaf(value: Any) extends DumpTree

object DumpTree {
  // If 'x' belongs to class Foo, call: DumpTree.build(x, scala.reflect.runtime.universe.typeOf[Foo])
  def build(obj: Any, obj_type: ru.Type): DumpTree = {
    obj match {
      case ls: List[Any] => Sequence(ls.map(
        // Thanks http://stackoverflow.com/questions/12842729
        x => build(x, obj_type.asInstanceOf[ru.TypeRefApi].args.head)
      ).toList)
      case ls: Array[_] => Sequence(ls.map(
        x => build(x, obj_type.asInstanceOf[ru.TypeRefApi].args.head)
      ).toList)
      case m: Map[_, _] => Mapping(m.map({
        case (k, v) => k.toString -> build(v, obj_type.asInstanceOf[ru.TypeRefApi].args.last)
      }))
      // case_class_fields breaks on strings, so a special case
      case x: String => Leaf(x)
      case _ => {
        // TODO Ideally, detect case classes from primitives. This seems to work.
        val fields = case_class_fields(obj_type)
        if (fields.isEmpty) {
          Leaf(obj)
        } else {
          Object(obj_type.toString, immutable.ListMap[String, DumpTree]() ++ (
            fields.keys.map(name => name -> build(get_field(obj, obj_type, name), fields(name)))
          ))
        }
      }
    }
  }

  // Thanks http://stackoverflow.com/questions/16079113
  private def case_class_fields(tpe: ru.Type): immutable.ListMap[String, ru.Type] = {
    val ctor = tpe.declaration(ru.nme.CONSTRUCTOR)
    val default_ctor =
      if (ctor.isMethod)
        ctor.asMethod
      else
        ctor.asTerm.alternatives.map(_.asMethod).find(_.isPrimaryConstructor).get

    return immutable.ListMap[String, ru.Type]() ++ default_ctor.paramss.reduceLeft(_ ++ _).map({
      sym => sym.name.toString -> tpe.member(sym.name).asMethod.returnType
    })
  }

  private def get_field(obj: Any, obj_tpe: ru.Type, field: String): Any = {
    val m = ru.runtimeMirror(obj.getClass.getClassLoader)
    val term = obj_tpe.declaration(ru.newTermName(field)).asTerm
    val im = m.reflect(obj)
    return im.reflectField(term).get
  }
}

object AsciiTreeViewer {
  def view(t: DumpTree) = show(t, 0, true)

  private def show(t: DumpTree, level: Int, indent: Boolean) {
    val header =
      if (indent)
        "  " * level
      else
        ""
    t match {
      case Object(name, fields) => {
        println(header + name)
        for ((field, value) <- fields) {
          print(("  " * (level + 1)) + field + " = ")
          show(value, level + 1, false)
        }
      }
      case Sequence(seq) => {
        println(header + "[")
        seq.foreach(x => show(x, level + 1, true))
        println(("  " * level) + "]")
      }
      case Mapping(m) => {
        println(header + "{")
        for ((k, v) <- m) {
          print(("  " * (level + 1)) + k + " -> ")
          show(v, level + 1, false)
        }
        println(("  " * level) + "}")
      }
      case Leaf(x) => println(header + x)
    }
  }
}
