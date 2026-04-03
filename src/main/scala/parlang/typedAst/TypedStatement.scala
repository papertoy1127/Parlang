package parlang.typedAst

import parlang.ast.{Ast, Type}
import parlang.runtime.Value


sealed trait TypedStmt extends Ast

case class TypedVarDec(binder: TypedPattern, expr: TypedExpr) extends TypedStmt
case class TypedFuncDec(id: String, resolvedName: String, params: List[TypedPattern], retTy: Type, body: TypedExpr) extends TypedStmt

sealed trait TypedExpr extends TypedStmt {
  def ty: Type
}

case class TypedLiteral(v: Value, ty: Type) extends TypedExpr
case class TypedIdentifier(id: String, resolvedName: String, ty: Type) extends TypedExpr
case class TypedTuple(exprs: List[TypedExpr], ty: Type) extends TypedExpr
case class TypedCall(func: TypedExpr, arg: TypedExpr, ty: Type) extends TypedExpr
case class TypedScope(stmts: List[TypedStmt], ty: Type) extends TypedExpr
case class TypedLambda(param: TypedPattern, retTy: Type, body: TypedExpr, ty: Type) extends TypedExpr
case class TypedConditional(cond: TypedExpr, thenExpr: TypedExpr, elseExpr: TypedExpr, ty: Type) extends TypedExpr
case class TypedTagged(expr: TypedExpr, ty: Type) extends TypedExpr

