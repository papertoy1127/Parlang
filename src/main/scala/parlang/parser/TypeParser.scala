package parlang.parser

import parlang.ast.*

trait TypeParser extends BaseParser {
  protected lazy val typeLiteral: Parser[TypeAtom] = positioned {
    "#" ~> tupleType(typeAtom) ^^ TaggedTy.apply |
      "?" ^^^ UnknownTy() |
      identifier ^^ {
        case "Int" => IntTy()
        case "Bool" => BoolTy()
        case "Char" => CharTy()
        case "String" => StringTy()
        case id => error(s"Unknown type: $id")
      }
  }

  protected lazy val tailType: Parser[Type] = positioned("*" ~> typeAtom ^^ Type.rep)

  protected lazy val typeBase: Parser[Type] = positioned {
    typeLiteral ^^ Type.single |
      "(" ~> eolOrNot ~> typeParser <~ eolOrNot <~ ")"
  }

  protected lazy val typeAtom: Parser[TypeAtom] = positioned {
    typeBase ~ opt("->" ~> typeParser) ^^ {
      case tPar ~ None =>
        tPar match {
          case Type.Exact(List(atom)) => atom
          case _ => error(s"Tuple type cannot be used as a single TypeAtom: $tPar")
        }
      case tPar ~ Some(tRes) => FuncTy(tPar, tRes)
    } |
      typeLiteral
  }

  protected lazy val exactTupleType: Parser[TypeAtom] => Parser[Type.Exact] =
    nextLayer => positioned {
      nextLayer ^^ Type.single |
        "(" ~> eolOrNot ~> repsep(exactTupleType(nextLayer), ",") <~ opt(",") <~ eolOrNot <~ ")" ^^ {
          t => Type.Exact(t.flatMap(_.prefix))
        }
    }

  protected lazy val restTupleType: Parser[TypeAtom] => Parser[Type] =
    nextLayer => positioned {
      "(" ~> eolOrNot ~> rep(exactTupleType(nextLayer) <~ ",") ~ restTupleType(nextLayer) <~ eolOrNot <~ ")"
        ^^ { case tList ~ tRest => Type.of(tList.flatMap(_.prefix) ++ tRest.prefix, tRest.rest) } |
        tailType
    }

  protected lazy val tupleType: Parser[TypeAtom] => Parser[Type] = nextLayer =>
    positioned {
      "(" ~> eolOrNot ~> (
        rep(exactTupleType(nextLayer) <~ ",") ~ restTupleType(nextLayer) ^^ {
          case pref ~ ty => Type.of(pref.flatMap(_.prefix) ++ ty.prefix, ty.rest)
        } |
          repsep(exactTupleType(nextLayer), ",") <~ opt(",") ^^ {
            parts => Type.Exact(parts.flatMap(_.prefix))
          }
        ) <~ eolOrNot <~ ")" |
        nextLayer ^^ Type.single
    }

  protected lazy val typeParser: Parser[Type] = tupleType(typeAtom)
}