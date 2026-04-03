package parlang.parser

import parlang.ast.*

trait StatementParser extends ExpressionParser {
  protected lazy val varDec: Parser[Variable] = positioned {
    "let" ~> pattern ~ (":=" ~> expr) ^^ { case p ~ e => Variable(p, e) }
  }

  protected lazy val funcDec: Parser[FuncRec] = positioned {
    "let" ~> identifier ~ rep1(pattern) ~ (eolOrNot ~> "->" ~> eolOrNot ~> typeParser) ~ (":=" ~> expr)
      ^^ { case id ~ params ~ retTy ~ e => FuncRec(id, params, retTy, e) }
  }

  protected lazy val statement: Parser[Statement] = varDec | funcDec | expr

  protected lazy val program: Parser[List[Statement]] = eolOrNot ~> repsep(statement, eol) <~ eolOrNot
}