package parlang.ast

import parlang.runtime.*

sealed trait Statement
case class Variable(binder: Pattern, expr: Expression) extends Statement
case class FuncRec(id: String, params: List[Pattern], retTy: Option[Type], body: Expression) extends Statement

sealed trait Expression extends Statement

case class Scope(statements: List[Statement]) extends Expression
case class Literal(value: Value) extends Expression
case class Lambda(param: Pattern, retTy: Option[Type], body: Expression) extends Expression
case class Identifier(s: String) extends Expression
case class Call(expr: Expression, arg: Expression) extends Expression
case class ExprTuple(elems: List[Expression]) extends Expression
case class ExprTagged(expr: Expression) extends Expression

case class OperatorUnary(op: UnaryOperType, operand: Expression) extends Expression
case class OperatorBinary(op: BinaryOperType, left: Expression, right: Expression) extends Expression
case class Conditional(cond: Expression, left: Expression, right: Expression) extends Expression

enum UnaryOperType:
  case Plus, Minus, LogicalNot

enum BinaryOperType:
  case Plus, Minus, Times, Over, LogicalAnd, LogicalOr, Equals, NotEquals
