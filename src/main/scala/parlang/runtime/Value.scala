package parlang.runtime

import parlang.ast.*

sealed trait ValAtom {
  override def toString: String = this match {
    case ValueNum(n) => n.toString
    case ValueBool(b) => b.toString
    case ValueChar(c) => s"'${Value.escape(c.toString)}'"
    case ValueString(s) => s"\"${Value.escape(s)}\""
    case ValueTagged(v) => s"#$v"
    case _: ValueClosure => "<function>"
    case ValueBuiltin(name, _) => s"<built-in function: $name>"
    case ValueRecClosure(id, _, _, _) => s"<function: $id>"
    case ValQuit => "<quit>"
  }
}

case class ValueNum(n: BigInt) extends ValAtom
case class ValueBool(b: Boolean) extends ValAtom
case class ValueChar(s: Char) extends ValAtom
case class ValueString(s: String) extends ValAtom
case class ValueTagged(v: Value) extends ValAtom

sealed trait CallableAtom extends ValAtom
case class ValueClosure(param: Pattern, expr: Expression, env: Map[String, Value]) extends CallableAtom
case class ValueRecClosure(id: String, param: Pattern, expr: Expression, env: Map[String, Value]) extends CallableAtom
case class ValueBuiltin(name: String, func: Value => Value) extends CallableAtom
case object ValQuit extends CallableAtom

case class Value(elems: List[ValAtom]) {
  override def toString: String = elems match {
    case Nil           => "()"
    case single :: Nil => single.toString
    case multiple      => multiple.mkString("(", ", ", ")")
  }
}

object Value:
  val Unit = Value(Nil)
  def singular(valAtom: ValAtom) = Value(valAtom :: Nil)

  def escape(s: String): String = s.flatMap {
    case '\n' => "\\n"
    case '\t' => "\\t"
    case '\r' => "\\r"
    case '\\' => "\\\\"
    case '\"' => "\\\""
    case '\'' => "\\\'"
    case c    => c.toString
  }