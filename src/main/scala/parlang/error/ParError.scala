package parlang.error

import parlang.ast.*
import parlang.runtime.Value
import scala.util.parsing.input.Position

sealed trait ParError {
  def message: String

  protected def errorName: String =
    this.getClass.getSimpleName //.replaceAll("([a-z])([A-Z]+)", "$1 $2")

  protected def format(msg: String, pos: Position): String = {
    val header = s"[$errorName] $msg\n"
    if (pos == null || pos.line <= 0) header
    else {
      val indentedCode = pos.longString.replace("\n", "\n    ")
      s"$header+-- At ${pos.line}:${pos.column}\n    $indentedCode\n"
    }
  }

  protected def formatTypeMismatch(expectedTy: String, providedTy: String, expectPos: Position, providePos: Position): String = {
    val header = s"[$errorName]\n"
    val comparison = s"Expected : $expectedTy\nProvided : $providedTy\n"


    def buildProvided(pos: Position): String = {
      if (pos == null || pos.line <= 0) ""
      else {
        val indentedCode = pos.longString.replace("\n", "\n    ")
        s"+-- Provided at ${pos.line}:${pos.column}\n    $indentedCode\n"
      }
    }

    def buildExpected(pos: Position): String = {
      if (pos == null || pos.line <= 0) ""
      else {
        val indentedCode = pos.longString.replace("\n", "\n    ")
        s"+-- Expected at ${pos.line}:${pos.column}\n|   $indentedCode\n"
      }
    }

    header + comparison + buildProvided(providePos) + (if (providePos == expectPos) "" else buildExpected(expectPos))
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

case class TypeMismatchError(expected: Type, provided: Type, node: Ast) extends ParError {
  override def message: String = formatTypeMismatch(expected.toString, provided.toString, expected.pos, node.pos)
}

case class UnknownTypeBindingError(id: String, ty: Type, node: Ast) extends ParError {
  override def message: String = format(s"cannot bind unknown type $ty to identifier $id", node.pos)
}

case class PatternMismatchError(expectedPat: Pattern, provided: Type, node: Ast) extends ParError {
  override def message: String = formatTypeMismatch(expectedPat.toString, provided.toString, expectedPat.pos, node.pos)
}

case class PatternShapeMismatchError(node: Ast) extends ParError {
  override def message: String = format("pattern and type structure mismatch", node.pos)
}

case class EndOfPatternError(expectedTail: AbsurdTail, provided: Type, node: Ast) extends ParError {
  override def message: String = formatTypeMismatch("∅ (End of Tuple)", provided.toString, null, node.pos)
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