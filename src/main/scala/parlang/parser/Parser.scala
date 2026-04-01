package parlang.parser

import parlang.ast.*

object Parser extends StatementParser {

  def apply(input: String): Statement = parseAll(statement, input).get

  def parseProgram(input: String): List[Statement] = {
    parseAll(program, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => error(failure.msg)
    }
  }

  enum ParseState:
    case Complete(stmt: List[Statement])
    case Incomplete
    case Error(msg: String)

  def parseRepl(input: String): ParseState = {
    parseAll(program, input) match {
      case Success(result, _) => ParseState.Complete(result)
      case failure: NoSuccess =>
        if (failure.next.atEnd) ParseState.Incomplete
        else ParseState.Error(s"${failure.msg} at line ${failure.next.pos.line}, column ${failure.next.pos.column}")
    }
  }
}