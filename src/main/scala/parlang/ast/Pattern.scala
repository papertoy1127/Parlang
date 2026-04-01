package parlang.ast

sealed trait PatAtom
case class TaggedPat(pat: Pattern) extends PatAtom

sealed trait PatLiteral extends PatAtom
case class IdPat(id: String, ty: Type) extends PatLiteral
case class DiscardPat(ty: Type) extends PatLiteral

sealed trait Pattern {
  def prefix: List[PatAtom]
}

case class ExactPattern(prefix: List[PatAtom]) extends Pattern
case class RestPattern(prefix: List[PatAtom], rest: PatLiteral) extends Pattern