package parlang.parser

import parlang.ast.*
import parlang.runtime.*

trait LiteralParser extends BaseParser {
  private def unescape(c: Char): Char = c match {
    case 'n' => '\n'
    case 't' => '\t'
    case 'r' => '\r'
    case '\\' => '\\'
    case '\'' => '\''
    case '\"' => '\"'
    case _ => error(s"Unknown escape literal: \\$c")
  }

  private def decode(s: String): String = {
    val escapeRegex = """\\.""".r
    escapeRegex.replaceAllIn(s, m => unescape(m.matched.charAt(1)).toString)
  }

  protected def stringLiteral: Parser[Expression] = positioned {
    """"([^"\\]|\\.)*"""".r ^^ { s =>
      val content = s.substring(1, s.length - 1)
      Literal(Value.singular(ValueString(decode(content))))
    }
  }

  protected def charLiteral: Parser[Expression] = positioned {
    """'([^'\\]|\\.)'""".r ^^ { s =>
      val content = s.substring(1, s.length - 1)
      Literal(Value.singular(ValueChar(decode(content).charAt(0))))
    }
  }

  protected lazy val valueLiteral: Parser[Expression] = positioned {
    charLiteral | stringLiteral |
    """\d+""".r ^^ { s => Literal(Value(ValueNum(BigInt(s)) :: Nil)) } |
    "true" ^^^ Literal(Value(ValueBool(true) :: Nil)) |
    "false" ^^^ Literal(Value(ValueBool(false) :: Nil))
  }
}