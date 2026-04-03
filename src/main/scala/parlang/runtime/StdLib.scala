/*

package parlang.runtime

import parlang.parser.Parser

object StdLib {
  def error(str: String): Nothing = throw Exception(s"[StdLib Error] $str")
  private val nativeEnv: Map[String, Value] = Map(
    "write" -> Value.singular(ValueBuiltin("write", { v =>
      v.elems match {
        case ValueString(s) :: Nil => print(s)
        case ValueChar(c) :: Nil => print(c.toString)
        case _ => error(s"$v.toString is not a string!")
      }

      Value.Unit
    })),
    "__builtin_toString" -> Value.singular(ValueBuiltin("__builtin_toString", {
      case Value((v: ValueString) :: Nil) => Value.singular(v)
      case Value(ValueChar(c) :: Nil) => Value.singular(ValueString(c.toString))
      case v => Value.singular(ValueString(v.toString))
    })),
    "len" -> Value.singular(ValueBuiltin("len", { v =>
      Value.singular(ValueNum(v.elems.length))
    })),
    "is_empty" -> Value.singular(ValueBuiltin("is_empty", { v =>
      Value.singular(ValueBool(v.elems.isEmpty))
    })),
    "quit" -> Value.singular(ValQuit)
  )

  val predefined = """
    let head (x, *rest) := x
    let tail (x, *rest) := rest

    let map f *tup := {
      if (is_empty tup) ()
      else (f(head tup), map f(tail tup))
    }

    let filter f *tup := {
      if (is_empty tup) ()
      else if (f(head tup)) (head tup, filter f(tail tup))
      else filter f(tail tup)
    }

    let print(*items) := {
      if (is_empty(items)) ()
      else if (is_empty(tail items)) {
        write(__builtin_toString(head items))
        write("\n")
      } else {
        write(__builtin_toString(head items))
        write(" ")
        print(tail items)
      }
    }

  """

  lazy val env: Map[String, Value] = {
    //val stmts = Parser.parseProgram(predefined)
    val stmts = Parser.parseProgram("")
    stmts.foldLeft(nativeEnv) { case (currentEnv, stmt) =>
      val (newEnv, _) = Interp.interp(stmt, currentEnv)
      newEnv
    }
  }
}


 */