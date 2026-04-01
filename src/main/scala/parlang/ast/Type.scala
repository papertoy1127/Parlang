package parlang.ast

sealed trait TypeAtom
case object IntTy extends TypeAtom
case object BoolTy extends TypeAtom
case object CharTy extends TypeAtom
case object StringTy extends TypeAtom
case class TaggedTy(t: Type) extends TypeAtom
case class FuncTy(param: Type, result: Type) extends TypeAtom

sealed trait Type {
  def prefix: List[TypeAtom]
}

case class ExactType(prefix: List[TypeAtom]) extends Type
case class RestType(prefix: List[TypeAtom], rest: TypeAtom) extends Type