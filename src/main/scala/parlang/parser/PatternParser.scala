package parlang.parser

import parlang.ast.*

trait PatternParser extends TypeParser {
  protected lazy val patSingle: Parser[SinglePatAtom] = positioned {
    "#" ~> patGroup ^^ TaggedPat.apply |
      "_" ~> opt(":" ~> typeAtom) ^^ { ty => WildcardPat(ty.getOrElse(UnknownTy())) } |
      identifier ~ opt(":" ~> typeAtom) ^^ { case id ~ ty => IdPat(id, ty.getOrElse(UnknownTy())) }
  }

  protected lazy val patVariadic: Parser[VariadicPatAtom] = positioned {
    "*" ~> "_" ~> (":" ~> exactTupleType(typeAtom)) ^^ { ty => VarWildcardPat(ty) } |
      "*" ~> identifier ~ (":" ~> exactTupleType(typeAtom)) ^^ { case id ~ ty => VarIdPat(id, ty) }
  }

  protected lazy val exactPat: Parser[Pattern.Exact] = positioned {
    patSingle ^^ Pattern.single | patVariadic ^^ Pattern.single |
      "(" ~> eolOrNot ~> repsep(exactPat, ",") <~ opt(",") <~ eolOrNot <~ ")"
        ^^ { t => Pattern.Exact(t.flatMap(_.prefix)) }
  }

  protected lazy val tailPat: Parser[PatTail] = positioned {
    "*" ~> "_" ~> opt(":" ~> tailType) ^^ { ty => WildcardTail(ty.getOrElse(Type.rep(UnknownTy()))) } |
      "*" ~> identifier ~ opt(":" ~> tailType) ^^ { case id ~ ty => IdTail(id, ty.getOrElse(Type.rep(UnknownTy()))) }
  }

  protected lazy val restPat: Parser[Pattern] = positioned {
    "(" ~> eolOrNot ~> rep(exactPat <~ ",") ~ restPat <~ eolOrNot <~ ")"
      ^^ { case pref ~ ty => Pattern.of(pref.flatMap(_.prefix) ++ ty.prefix, ty.rest)} |
      tailPat ^^ Pattern.rep
  }

  protected lazy val patGroup: Parser[Pattern] = exactPat | restPat

  protected lazy val pattern: Parser[Pattern] = patGroup
}