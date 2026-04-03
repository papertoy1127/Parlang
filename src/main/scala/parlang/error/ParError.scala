package parlang.error

import parlang.ast.*
import parlang.runtime.Value
import scala.util.parsing.input.Position

sealed trait ParError {
  def message: String
  protected def format(msg: String, pos: Position): String = {
    if (pos == null || pos.line == 0) msg
    else s"[Line ${pos.line}, Column ${pos.column}] $msg\n${pos.longString}"
  }
}

case class ParseError(msg: String, pos: Ast) extends ParError {
  override def message: String = format(msg, pos.pos)
}

case class LookupError(id: String, node: Ast) extends ParError {
  override def message: String = format(s"unbound identifier: $id", node.pos)
}

case class AmbiguousLookupError(id: String, node: Ast) extends ParError {
  override def message: String = format(s"ambiguous identifier: $id", node.pos)
}

case class TypeMismatchError(expected: Type, provided: Type) extends ParError {
  override def message: String =
    format(s"provided $provided is not a subtype of expected $expected", provided.pos)
}

case class UnknownTypeBindingError(id: String, ty: Type, node: Ast) extends ParError {
  override def message: String = format(s"cannot bind unknown type $ty to identifier $id", node.pos)
}

case class PatternMismatchError(expectedPat: Pattern, provided: Type) extends ParError {
  override def message: String = format(s"expected $expectedPat, but got $provided", provided.pos)
}

case class PatternShapeMismatchError(node: Ast) extends ParError {
  override def message: String = format("pattern and type structure mismatch", node.pos)
}

case class EndOfPatternError(expectedTail: AbsurdTail, provided: Type) extends ParError {
  override def message: String = format(s"expected end of pattern, but got $provided", provided.pos)
}

case class VariadicSegmentError(id: String, provided: Type, node: Ast) extends ParError {
  override def message: String = format(s"variadic segment pattern (*$id) must match a fixed-length segment, but got $provided", node.pos)
}

case class TupleElementCountError(msg: String, node: Ast) extends ParError {
  override def message: String = format(msg, node.pos)
}

case class NotCallableError(provided: Type, node: Ast) extends ParError {
  override def message: String = format(s"expression is not callable: $provided", node.pos)
}

case class VariadicPlacementError(ty: Type, node: Ast) extends ParError {
  override def message: String = format(s"expression with variadic type $ty cannot appear in the middle of a tuple", node.pos)
}

case class IncompatibleBranchesError(thenTy: Type, elseTy: Type, node: Ast) extends ParError {
  override def message: String = format(s"conditional branches have incompatible types: then=$thenTy, else=$elseTy", node.pos)
}

case class EmptyParameterError(id: String, node: Ast) extends ParError {
  override def message: String = format(s"function '$id' must have at least one parameter list", node.pos)
}

case class ComplexValueInferenceError(v: Value, node: Ast) extends ParError {
  override def message: String = format("cannot infer type of complex runtime value during static analysis", node.pos)
}

case class ConcatError(t1: Type, node: Ast) extends ParError {
  override def message: String = format(s"cannot place elements after a variadic type: $t1", node.pos)
}

case class RuntimeError(msg: String, node: Ast) extends ParError {
  override def message: String = format(msg, node.pos)
}