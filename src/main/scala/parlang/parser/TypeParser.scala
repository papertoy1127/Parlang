package parlang.parser

import parlang.ast.*

trait TypeParser extends BaseParser {
  protected lazy val typeAtom: Parser[TypeAtom] =
    "#" ~> baseType ^^ { t => TaggedTy(t) } |
    identifier ^^ {
      case "Int" => IntTy
      case "Bool" => BoolTy
      case "Char" => CharTy
      case "String" => StringTy
      case typeId => error(s"Unknown type or custom type not yet supported: $typeId")
    }

  protected lazy val baseType: Parser[Type] = {
    "(" ~ eolOrNot ~ ")" ^^^ ExactType(Nil) |
    "(" ~> eolOrNot ~> typeParser <~ eolOrNot <~ ")" |
    typeAtom ^^ { a => ExactType(a :: Nil) }
  }

  protected lazy val singleType: Parser[Type] = {
    val variadicOrBase = "*" ~> baseType ^^ {
      case ExactType(atom :: Nil) => RestType(Nil, atom)
      case other => error(s"Variadic type (*) must be a single type atom, but got $other")
    } | baseType

    variadicOrBase ~ opt(eolOrNot ~> "->" ~> eolOrNot ~> singleType) ^^ {
      case p ~ Some(r) => ExactType(FuncTy(p, r) :: Nil)
      case single ~ None => single
    }
  }

  protected lazy val tupleType: Parser[Type] => Parser[Type] = { nextLayer =>
    "*" ~> nextLayer ^^ {
      case ExactType(atom :: Nil) => RestType(Nil, atom)
      case other => error(s"Variadic type (*) must be a single type atom, but got $other")
    } |
    rep1sep(nextLayer, eolOrNot ~> "," <~ eolOrNot) ~ opt(eolOrNot ~> "," ~> eolOrNot ~> "*" ~> nextLayer) ^^ {
      case types ~ optRest =>
        val flatPrefix = types.flatMap {
          case ExactType(elems) => elems
          case RestType(_, _) => error("Variadic type (*) can only appear at the end of a tuple type")
        }
        optRest match {
          case Some(ExactType(List(variadicAtom))) => RestType(flatPrefix, variadicAtom)
          case Some(other) => error(s"Variadic type (*) must be a single type atom, but got $other")
          case None => ExactType(flatPrefix)
        }
    }
  }

  protected lazy val funcType: Parser[Type] => Parser[Type] = { nextLayer =>
    nextLayer ~ opt(eolOrNot ~> "->" ~> eolOrNot ~> typeParser) ^^ {
      case p ~ Some(r) => ExactType(FuncTy(p, r) :: Nil)
      case single ~ None => single
    }
  }

  protected val typeLayers: List[Parser[Type] => Parser[Type]] =
    tupleType ::
    funcType ::
    Nil

  protected lazy val typeParser: Parser[Type] = typeLayers.foldLeft(baseType) { (p, layerFunc) => layerFunc(p) }
}