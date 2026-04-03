package parlang.ast

sealed trait PatAtom extends Ast

sealed trait SinglePatAtom extends PatAtom {
  def ty: TypeAtom
}

sealed trait VariadicPatAtom extends PatAtom {
  def ty: Type.Exact
}

case class IdPat(id: String, ty: TypeAtom) extends SinglePatAtom
case class WildcardPat(ty: TypeAtom) extends SinglePatAtom
case class TaggedPat(pat: Pattern) extends SinglePatAtom {
  override lazy val ty: TypeAtom = TaggedTy(pat.typeConstraint)
}

case class VarIdPat(id: String, ty: Type.Exact) extends VariadicPatAtom
case class VarWildcardPat(ty: Type.Exact) extends VariadicPatAtom

sealed trait PatTail extends Ast {
  def ty: Type
}

case class IdTail(id: String, ty: Type) extends PatTail
case class WildcardTail(ty: Type) extends PatTail
case class AbsurdTail() extends PatTail {
  override val ty: Type = Type.unit
}

sealed trait Pattern extends Ast {
  def prefix: List[PatAtom]
  def rest: PatTail

  lazy val typeConstraint: Type = {
    val pTys = prefix.flatMap {
      case si: SinglePatAtom => si.ty :: Nil
      case va: VariadicPatAtom => va.ty.prefix
    }
    this match {
      case Pattern.Exact(_) => Type.Exact(pTys).at(this)
      case Pattern.Rest(_, r) =>
        Type.of(pTys ++ r.ty.prefix, r.ty.rest).at(this)
    }
  }
}

object Pattern {
  def of(prefix: List[PatAtom], rest: PatTail): Pattern = rest match {
    case AbsurdTail() => Exact(prefix).at(rest)
    case _ => Rest(prefix, rest).at(rest)
  }

  def rep(tail: PatTail) = Rest(Nil, tail).at(tail)
  def single(atom: PatAtom) = Exact(atom :: Nil).at(atom)
  def unit = Exact(Nil)

  case class Exact(prefix: List[PatAtom]) extends Pattern {
    override val rest: PatTail = AbsurdTail()
  }
  case class Rest(prefix: List[PatAtom], rest: PatTail) extends Pattern

  def unapply(p: Pattern): Option[(List[PatAtom], PatTail)] = Some((p.prefix, p.rest))
}