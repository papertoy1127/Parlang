package parlang.parser

import parlang.ast.*
import parlang.runtime.*

trait ExpressionParser extends LiteralParser with PatternParser {
  protected def statement: Parser[Statement]

  protected lazy val scope: Parser[Expression] = "{" ~> eolOrNot ~> repsep(statement, eol) <~ eolOrNot <~ "}" ^^ Scope.apply

  protected lazy val lambda: Parser[Lambda] =
    singleParam ~ opt(eolOrNot ~> "->" ~> eolOrNot ~> singleType) ~ ("=>" ~> expr) ^^ { case param ~ retTy ~ e => Lambda(param, retTy, e) }

  protected lazy val conditional: Parser[Expression] =
    ("if" ~> "(" ~> expr <~ ")" <~ eolOrNot) ~ expr ~ opt(eolOrNot ~> "else" ~> eolOrNot ~> expr) ^^ {
      case cond ~ thenExpr ~ Some(elseExpr) => Conditional(cond, thenExpr, elseExpr)
      case cond ~ thenExpr ~ None => Conditional(cond, thenExpr, Literal(Value.Unit))
    }

  protected lazy val unit: Parser[Expression] = "(" ~ eolOrNot ~ ")" ^^ (_ => Literal(Value.Unit))
  protected lazy val tag: Parser[Expression] = "#" ~> atom ^^ ExprTagged.apply
  protected lazy val atom: Parser[Expression] = tag | scope | lambda | conditional | valueLiteral | identifier ^^ Identifier.apply | unit | "(" ~> eolOrNot ~> expr <~ eolOrNot <~ ")"

  private lazy val call: Parser[Expression] => Parser[Expression] = {
    nextLayer =>
      nextLayer ~ rep(nextLayer) ^^ {
        case fn ~ args =>
          args.foldLeft(fn) { case (func, arg) => Call(func, arg) }
      }
  }

  private def unary(ops: Map[String, UnaryOperType]): Parser[Expression] => Parser[Expression] = {
    nextLayer =>
      val opParser = ops.keys.map(literal).reduce(_ | _)
      rep(opParser) ~ nextLayer ^^ {
        case opList ~ expr => opList.foldRight(expr) { (op, acc) => OperatorUnary(ops(op), acc) }
      }
  }

  private def binary(ops: Map[String, BinaryOperType]): Parser[Expression] => Parser[Expression] = {
    nextLayer =>
      val opParser = ops.keys.map(literal).reduce(_ | _)
      nextLayer ~ rep(opParser ~ nextLayer) ^^ {
        case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => OperatorBinary(ops(op), l, r) }
      }
  }

  private lazy val ternary: Parser[Expression] => Parser[Expression] = {
    nextLayer =>
      nextLayer ~ opt(("?" ~> expr) ~ (":" ~> expr)) ^^ {
        case cond ~ Some(thenExpr ~ elseExpr) => Conditional(cond, thenExpr, elseExpr)
        case expr ~ None => expr
      }
  }

  private lazy val tuple: Parser[Expression] => Parser[Expression] = {
    nextLayer =>
      rep1sep(nextLayer, eolOrNot ~ "," ~ eolOrNot) <~ opt(",") ^^ {
        case single :: Nil => single
        case multiple => ExprTuple(multiple)
      }
  }

  private val layers: List[Parser[Expression] => Parser[Expression]] =
    call ::
    unary(Map("+" -> UnaryOperType.Plus, "-" -> UnaryOperType.Minus, "!" -> UnaryOperType.LogicalNot)) ::
    binary(Map("*" -> BinaryOperType.Times, "/" -> BinaryOperType.Over)) ::
    binary(Map("+" -> BinaryOperType.Plus,  "-" -> BinaryOperType.Minus)) ::
    tuple ::
    binary(Map("=" -> BinaryOperType.Equals, "!=" -> BinaryOperType.NotEquals)) ::
    binary(Map("&&" -> BinaryOperType.LogicalAnd)) ::
    binary(Map("||" -> BinaryOperType.LogicalOr)) ::
    ternary ::
    Nil

  protected lazy val expr: Parser[Expression] = layers.foldLeft(atom) { (p, layerFunc) => layerFunc(p) }
}