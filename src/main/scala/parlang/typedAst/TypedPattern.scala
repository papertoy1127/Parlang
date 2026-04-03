package parlang.typedAst

import parlang.ast.*

sealed trait TypedPatAtom extends Ast {
  def ty: Type.Exact
}

sealed trait TypedSinglePatAtom extends TypedPatAtom {
  override val ty: Type.Exact = Type.single(tyAtom)
  def tyAtom: TypeAtom
}

sealed trait TypedVariadicPatAtom extends TypedPatAtom {
  override def ty: Type.Exact
}

case class TypedIdPat(id: String, resolvedName: String, tyAtom: TypeAtom) extends TypedSinglePatAtom
case class TypedWildcardPat(tyAtom: TypeAtom) extends TypedSinglePatAtom
case class TypedTaggedPat(inner: TypedPattern, tyAtom: TypeAtom) extends TypedSinglePatAtom

case class TypedVarIdPat(id: String, resolvedName: String, ty: Type.Exact) extends TypedVariadicPatAtom
case class TypedVarWildcardPat(ty: Type.Exact) extends TypedVariadicPatAtom
case class TypedVarTaggedPat(inner: TypedPattern, ty: Type.Exact) extends TypedVariadicPatAtom

sealed trait TypedPatTail extends Ast {
  def ty: Type
}

case class TypedIdTail(id: String, resolvedName: String, ty: Type) extends TypedPatTail
case class TypedWildcardTail(ty: Type) extends TypedPatTail
case object TypedAbsurdTail extends TypedPatTail {
  override val ty: Type = Type.unit
}

case class TypedPattern(prefix: List[TypedPatAtom], rest: TypedPatTail, ty: Type) extends Ast