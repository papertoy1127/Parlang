package parlang.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.*

trait BaseParser extends RegexParsers {
  def error(str: String): Nothing = throw Exception(s"[Parsing Error] $str")

  override val whiteSpace: Regex = """([ \t\r]|//.*|/\*[\s\S]*?\*/)+""".r

  protected def eol: Parser[Unit] = rep1(";" | """(?:\r\n|\r|\n)+""".r) ^^^ ()
  protected def eolOrNot: Parser[Unit] = rep(";" | """(?:\r\n|\r|\n)+""".r) ^^^ ()

  protected def reserved: Parser[String] = "if" | "else" | "let" | "true" | "false"
  protected def identifier: Parser[String] = not(reserved) ~> """[a-zA-Z_]\w*""".r
}