package parlang.ast

import scala.util.parsing.input.Positional

trait Ast extends Positional {
  def at(other: Ast): this.type = {
    this.setPos(other.pos)
    this
  }
}

case object BuiltinBase extends Ast {
  override def toString: String = "Builtin"
}