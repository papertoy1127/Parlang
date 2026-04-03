package parlang.ast

sealed trait TypeAtom extends Ast {
  def isSubAt(other: TypeAtom): Boolean = (this, other) match {
    case (NothingTy(), _) => true
    case (_, UnknownTy()) => true
    case (IntTy(), IntTy()) => true
    case (BoolTy(), BoolTy()) => true
    case (CharTy(), CharTy()) => true
    case (StringTy(), StringTy()) => true
    case (TaggedTy(inThis), TaggedTy(inOther)) => inThis.isSubTy(inOther)
    case (FuncTy(parThis, resThis), FuncTy(parOther, resOther)) => parOther.isSubTy(parThis) && resThis.isSubTy(resOther)
    case _ => false
  }

  def isKnown: Boolean = this match {
    case UnknownTy() => false
    case TaggedTy(innerTy) => innerTy.isKnown
    case FuncTy(param, result) => param.isKnown && result.isKnown
    case _ => true
  }

  override def toString: String = this match {
    case IntTy() => "Int"
    case BoolTy() => "Bool"
    case CharTy() => "Char"
    case StringTy() => "String"
    case UnknownTy() => "?"
    case NothingTy() => "∅"

    case FuncTy(param, ret) =>
      val paramStr = param match {
        case Type.Exact(List(f: FuncTy)) => s"(${f.toString})"
        case _ => param.toString
      }
      s"$paramStr -> ${ret.toString}"

    case TaggedTy(t) =>
      val inner = t.toString
      s"#$inner"

    case other => other.toString
  }
}

case class IntTy() extends TypeAtom
case class BoolTy() extends TypeAtom
case class CharTy() extends TypeAtom
case class StringTy() extends TypeAtom
case class TaggedTy(t: Type) extends TypeAtom
case class FuncTy(param: Type, result: Type) extends TypeAtom

case class NothingTy() extends TypeAtom
case class UnknownTy() extends TypeAtom


sealed trait Type extends Ast {
  def prefix: List[TypeAtom]
  def rest: TypeAtom
  def isSubTy(other: Type): Boolean = {
    val (p1, r1) = (this.prefix, this.rest)
    val (p2, r2) = (other.prefix, other.rest)

    p1.length >= p2.length &&
      p1.take(p2.length).zip(p2).forall { (t1, t2) => t1.isSubAt(t2) } &&
      p1.drop(p2.length).forall(_.isSubAt(r2)) &&
      r1.isSubAt(r2)
  }

  lazy val isKnown: Boolean = prefix.forall(_.isKnown) && rest.isKnown

  override def toString: String = this match {
    case Type.Exact(Nil) => "()"
    case Type.Exact(List(single)) => single.toString
    case Type.Exact(list) => list.map(_.toString).mkString("(", ", ", ")")

    case Type.Rest(Nil, atom) => s"*${atom.toString}"
    case Type.Rest(prefix, atom) =>
      prefix.map(_.toString).mkString("(", ", ", s", *${atom.toString})")
  }
}

object Type {
  def single(ty: TypeAtom) = Exact(ty :: Nil).setPos(ty.pos)
  def rep(ty: TypeAtom) = of(Nil, ty).setPos(ty.pos)
  def unit = Exact(Nil)

  def of(prefix: List[TypeAtom], rest: TypeAtom = NothingTy()): Type = rest match {
    case NothingTy() => Exact(prefix)
    case _ => Rest(prefix, rest)
  }

  def unapply(t: Type): Option[(List[TypeAtom], TypeAtom)] = Some((t.prefix, t.rest))

  case class Exact(prefix: List[TypeAtom]) extends Type {
    override val rest: TypeAtom = NothingTy()
  }

  case class Rest(prefix: List[TypeAtom], rest: TypeAtom) extends Type
}