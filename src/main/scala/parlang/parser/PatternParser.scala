package parlang.parser

import parlang.ast.*

trait PatternParser extends TypeParser {
  protected lazy val patLiteral: Parser[PatLiteral] =
    "_" ~> ":" ~> singleType ^^ DiscardPat.apply |
    identifier ~ (":" ~> singleType) ^^ { case id ~ ty => IdPat(id, ty) }

  protected lazy val patAtom: Parser[PatAtom] =
    "#" ~> patGroup ^^ TaggedPat.apply | patLiteral

  protected lazy val patGroup: Parser[Pattern] =
    "(" ~ eolOrNot ~ ")" ^^^ ExactPattern(Nil) |
    "(" ~> eolOrNot ~> pattern <~ eolOrNot <~ ")" |
    patAtom ^^ { a => ExactPattern(List(a)) }

  protected lazy val singleParam: Parser[Pattern] =
    patGroup | "*" ~> patLiteral ^^ { lit => RestPattern(Nil, lit) }

  protected lazy val pattern: Parser[Pattern] =
    "*" ~> patLiteral ^^ { lit => RestPattern(Nil, lit) } |
    rep1sep(patGroup, eolOrNot ~> "," <~ eolOrNot) ~ opt(eolOrNot ~> "," ~> eolOrNot ~> "*" ~> patLiteral) ^^ {
      case groups ~ optRest =>
        val lastIsRest = groups.lastOption.exists(_.isInstanceOf[RestPattern])
        val restInMiddle = groups.dropRight(1).exists(_.isInstanceOf[RestPattern])
        if (restInMiddle || (lastIsRest && optRest.isDefined)) {
          error("Variadic pattern (*) can only appear at the very end of the tuple pattern")
        }

        if (lastIsRest) {
          val restPat = groups.last.asInstanceOf[RestPattern]
          val flatPrefix = groups.dropRight(1).flatMap(_.prefix) ::: restPat.prefix
          RestPattern(flatPrefix, restPat.rest)
        } else {
          val flatPrefix = groups.flatMap(_.prefix)
          optRest match {
            case Some(lit) => RestPattern(flatPrefix, lit)
            case None => ExactPattern(flatPrefix)
          }
        }
    }
}