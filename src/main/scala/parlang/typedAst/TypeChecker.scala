package parlang.typedAst

import cats.data.{Chain, RWS}
import parlang.ast.{Pattern, *}
import parlang.error.*
import parlang.runtime.*
import cats.implicits.*

object TypeChecker {
  private def inferValueType(v: Value, node: Ast): Either[ParError, Type] = {
    v.elems.traverse {
      case ValueNum(_) => Right(IntTy().at(node))
      case ValueBool(_) => Right(BoolTy().at(node))
      case ValueChar(_) => Right(CharTy().at(node))
      case ValueString(_) => Right(StringTy().at(node))
      case ValueTagged(inner) => inferValueType(inner, node).map(ty => TaggedTy(ty).at(node))
      case _ => Left(ComplexValueInferenceError(v, node))
    }.map(tList => Type.of(tList, NothingTy()).at(node))
  }

  private def unifyAtom(provided: TypeAtom, expected: TypeAtom): Either[ParError, TypeAtom] = (provided, expected) match {
    case (UnknownTy(), e) => Right(e.at(provided))
    case (p, UnknownTy()) => Right(p)

    case (FuncTy(pProv, rProv), FuncTy(pExp, rExp)) =>
      for {
        uniParam <- unify(pProv, pExp)
        uniRet   <- unify(rProv, rExp)
      } yield FuncTy(uniParam, uniRet).at(provided)

    case (TaggedTy(tProv), TaggedTy(tExp)) =>
      unify(tProv, tExp).map(ty => TaggedTy(ty).at(provided))

    case _ =>
      if (!provided.isSubAt(expected))
        Left(TypeMismatchError(
          Type.single(expected).at(expected),
          Type.single(provided).at(provided)
        ))
      else Right(expected.at(provided))
  }

  private def unify(provided: Type, expected: Type): Either[ParError, Type] = (provided, expected) match {
    case (Type(p1, r1), Type(p2, r2)) if p1.length >= p2.length =>
      for {
        uniPrefix <- p1.take(p2.length).zip(p2).traverse { case (prov, exp) => unifyAtom(prov, exp) }
        inferredExtra <- p1.drop(p2.length).traverse(unifyAtom(_, r2))
        uniRest <- unifyAtom(r1, r2)
      } yield {
        val res = if (r2.isKnown) Type.of(uniPrefix, uniRest)
        else Type.of(uniPrefix ++ inferredExtra, uniRest)
        res.at(provided)
      }

    case _ => Left(TypeMismatchError(expected, provided))
  }

  private def bindAtom(pat: SinglePatAtom, tat: TypeAtom, env: TypeEnv): Either[ParError, (TypeEnv, TypedSinglePatAtom)] = pat match {
    case IdPat(id, chkTy) =>
      for {
        uniTy <- unifyAtom(tat, chkTy)
        (newEnv, info) <- env.bindVar(id, Type.single(uniTy).at(pat), pat)
      } yield (newEnv, TypedIdPat(id, info.resolvedName, uniTy))

    case WildcardPat(chkTy) =>
      unifyAtom(tat, chkTy).map { uniTy => (env, TypedWildcardPat(uniTy)) }

    case tagged @ TaggedPat(innerPat) =>
      tat match {
        case TaggedTy(innerTy) =>
          bind(innerPat, innerTy, env).map { case (nextEnv, typedInner) =>
            (nextEnv, TypedTaggedPat(typedInner, TaggedTy(typedInner.ty).at(tagged)))
          }
        case prov => Left(PatternMismatchError(Pattern.single(tagged).at(tagged), Type.single(prov).at(prov)))
      }
  }

  private def bindVariadicAtom(pat: VariadicPatAtom, ty: Type.Exact, env: TypeEnv): Either[ParError, (TypeEnv, TypedVariadicPatAtom)] = pat match {
    case VarIdPat(id, expected) =>
      unify(ty, expected).flatMap {
        case t @ Type(p, NothingTy()) =>
          val exactTy = Type.Exact(p).at(t)
          env.bindVar(id, exactTy, pat).map { case (newEnv, info) =>
            (newEnv, TypedVarIdPat(id, info.resolvedName, exactTy))
          }
        case otherTy => Left(VariadicSegmentError(id, otherTy, pat))
      }

    case VarWildcardPat(expected) =>
      unify(ty, expected).map { case uniTy: Type.Exact => (env, TypedVarWildcardPat(uniTy)) }
  }

  private def bindTail(pat: PatTail, ty: Type, env: TypeEnv): Either[ParError, (TypeEnv, TypedPatTail)] = (pat, ty) match {
    case (IdTail(id, chkTy), _) =>
      for {
        uniTy <- unify(ty, chkTy.at(pat))
        (nextEnv, info) <- env.bindVar(id, uniTy, pat)
      } yield (nextEnv, TypedIdTail(id, info.resolvedName, uniTy))

    case (WildcardTail(chkTy), _) =>
      unify(ty, chkTy.at(pat)).map { uniTy => (env, TypedWildcardTail(uniTy)) }

    case (eop: AbsurdTail, _) =>
      if (ty == Type.unit) Right((env, TypedAbsurdTail))
      else Left(EndOfPatternError(eop, ty))
  }

  def bind(binder: Pattern, dataTy: Type, env: TypeEnv): Either[ParError, (TypeEnv, TypedPattern)] = {
    for {
      refinedTy <- unify(dataTy, binder.typeConstraint).map(_.at(binder))
      result <- (binder, refinedTy) match {
        case (Pattern(pPats, rPat), Type(pData, rData)) =>
          for {
            (envAfter, tyPref, leftover) <- pPats.foldLeftM((env, Chain.empty[TypedPatAtom], pData)) {
              case ((currEnv, acc, rem), pat) =>
                pat match {
                  case s: SinglePatAtom =>
                    if (rem.isEmpty) Left(TupleElementCountError("Not enough elements in tuple for SinglePatAtom", binder))
                    else for (nextEnv, tAt) <- bindAtom(s, rem.head, currEnv)
                      yield (nextEnv, acc :+ tAt, rem.tail)

                  case v: VariadicPatAtom =>
                    val len = v.ty.prefix.length
                    if (rem.length < len) Left(TupleElementCountError(s"Not enough elements for variadic segment: expected $len", binder))
                    else {
                      val seg = rem.take(len)
                      for (nextEnv, tAt) <- bindVariadicAtom(v, Type.Exact(seg).at(v), currEnv)
                        yield (nextEnv, acc :+ tAt, rem.drop(len))
                    }
                }
            }
            tailData = Type.of(leftover, refinedTy.rest).at(rPat)
            (finalEnv, typedTail) <- bindTail(rPat, tailData, envAfter)
          } yield (finalEnv, TypedPattern(tyPref.toList, typedTail, refinedTy))

        case _ => Left(PatternShapeMismatchError(binder))
      }
    } yield result
  }

  def check(stmt: Statement, env: TypeEnv): Either[ParError, (TypeEnv, TypedStmt)] = stmt match {
    case Variable(binder, expr) =>
      checkExpr(expr, env, binder.typeConstraint.at(binder)).flatMap { typedExpr =>
        bind(binder, typedExpr.ty, env).map { (newEnv, typedPat) => (newEnv, TypedVarDec(typedPat, typedExpr)) }
      }

    case FuncRec(id, params, retTy, body) =>
      if (params.isEmpty) return Left(EmptyParameterError(id, stmt))

      val curriedFuncTy = params.init.foldRight(FuncTy(params.last.typeConstraint, retTy)) {
        (pat, accFuncTy) => FuncTy(pat.typeConstraint, Type.single(accFuncTy))
      }

      for {
        (envWithSelf, funcInfo) <- env.bindFunc(id, curriedFuncTy, stmt)
        (bodyEnv, typedParams) <- params.foldLeftM((envWithSelf.enterScope(), Chain.empty[TypedPattern])) {
          case ((currEnv, accPats), param) =>
            bind(param, param.typeConstraint, currEnv).map { case (nxtEnv, tPat) => (nxtEnv, accPats :+ tPat) }
        }
        typedBody <- checkExpr(body, bodyEnv, retTy)
      } yield (envWithSelf, TypedFuncDec(id, funcInfo.resolvedName, typedParams.toList, retTy, typedBody))

    case expr: Expression => checkExpr(expr, env, Type.rep(UnknownTy()).at(expr)).map(te => (env, te))
  }

  def estimateTypeLen(ty: Type): Option[Int] = ty match {
    case Type(list, NothingTy()) => Some(list.length)
    case _ => None
  }

  def estimateLength(expr: Expression, env: TypeEnv): Option[Int] = expr match {
    case Scope(statements) => statements.length match {
      case 0 => Some(0)
      case 1 => Some(1)
      case _ => None
    }
    case Literal(value) => Some(value.elems.length)
    case Lambda(_, _, _) => Some(1)
    case Identifier(s) =>
      val lookup = env.lookup(s)
      if (lookup.length == 1) estimateTypeLen(lookup.head.ty)
      else None
    case ExprTuple(elems) => elems.foldLeftM(0) { (acc, e) => estimateLength(e, env).map(x => x + acc) }
    case ExprTagged(_) => Some(1)
    case Call(Identifier(id), _) =>
      val lengths = env.lookup(id).flatMap {
        case FuncInfo(_, _, _, retTy) => estimateTypeLen(retTy)
        case VarInfo(Type(FuncTy(_, retTy) :: Nil, _), _) => estimateTypeLen(retTy)
        case _ => None
      }.distinct
      if (lengths.length == 1) Some(lengths.head)
      else None
    case Conditional(_, thenExpr, elseExpr) =>
      estimateLength(thenExpr, env).flatMap { tLen => estimateLength(elseExpr, env).filter(eLen => tLen == eLen) }
    case _ => None
  }

  def sliceType(t: Type, n: Int): (Type, Type) = {
    if (t.prefix.length >= n) (Type.of(t.prefix.take(n), NothingTy()).at(t), Type.of(t.prefix.drop(n), t.rest).at(t))
    else (Type.of(t.prefix ++ List.fill(n - t.prefix.length)(t.rest), NothingTy()).at(t), Type.rep(t.rest).at(t))
  }

  private def concatTypes(t1: Type, t2: Type, node: Ast): Either[ParError, Type] = (t1, t2) match {
    case (Type(p1, NothingTy()), Type(p2, r2)) => Right(Type.of(p1 ++ p2, r2).at(node))
    case (Type(p1, r1), _) => Left(ConcatError(t1, node))
  }

  def checkExpr(expr: Expression, env: TypeEnv, expected: Type): Either[ParError, TypedExpr] = expr match {
    case Literal(v) =>
      for {
        inf <- inferValueType(v, expr)
        ref <- unify(inf, expected)
      } yield TypedLiteral(v, ref.at(expr))

    case Identifier(id) => {
      val lookup = env.lookup(id)
      lookup.collect(sy => unify(sy.ty.at(expr), expected) match { case Right(ty) => (sy, ty) }) match {
        case Nil if lookup.isEmpty => Left(LookupError(id, expr))
        case Nil => Left(TypeMismatchError(expected, lookup.head.ty.at(expr)))
        case (rSy, rTy) :: Nil => Right(TypedIdentifier(id, rSy.resolvedName, rTy.at(expr)))
        case _ => Left(AmbiguousLookupError(id, expr))
      }
    }

    case Scope(stmts) =>
      stmts.dropRight(1).foldLeftM(env.enterScope(), Chain.empty[TypedStmt]) {
        case ((currentEnv, accStmts), stmt) => check(stmt, currentEnv).map {
          (nextEnv, typedStmt) => (nextEnv, accStmts :+ typedStmt)
        }
      }.flatMap { case (innerEnv, chn) =>
        stmts.lastOption match {
          case None => Right(TypedScope(chn.toList, Type.unit.at(expr)))
          case Some(e: Expression) => checkExpr(e, innerEnv, expected).map {
            tyExpr => TypedScope((chn :+ tyExpr).toList, tyExpr.ty)
          }
          case Some(s) => check(s, innerEnv).map {
            (_, tyStmt) => TypedScope((chn :+ tyStmt).toList, Type.unit.at(expr))
          }
        }
      }

    case OperatorUnary(op, operand) => checkExpr(Call(Identifier(op.id).at(expr), operand).at(expr), env, expected)
    case OperatorBinary(op, left, right) => checkExpr(Call(Identifier(op.id).at(expr), ExprTuple(left :: right::Nil).at(expr)).at(expr), env, expected)

    case Call(funcExpr, argExpr) =>
      checkExpr(funcExpr, env, Type.single(FuncTy(Type.rep(UnknownTy()).at(expr), expected).at(expr)).at(expr)).flatMap {
        e =>
          e.ty match {
            case Type(FuncTy(parTy, retTy) :: Nil, NothingTy()) =>
              checkExpr(argExpr, env, parTy).flatMap(tyArg => Right(TypedCall(e, tyArg, retTy)))
            case _ => Left(NotCallableError(e.ty, funcExpr))
          }
      }.left.flatMap {
        case _: LookupError | _: TypeMismatchError =>
          checkExpr(argExpr, env, Type.rep(UnknownTy()).at(argExpr)).flatMap {
            tyArg => {
              checkExpr(funcExpr, env, Type.single(FuncTy(tyArg.ty, expected).at(expr)).at(expr)).flatMap {
                e =>
                  e.ty match {
                    case Type(FuncTy(_, retTy) :: Nil, NothingTy()) => Right(TypedCall(e, tyArg, retTy))
                    case _ => Left(NotCallableError(e.ty, funcExpr))
                  }
              }
            }
          }
        case l: ParError => Left(l)
      }

    case Lambda(param, retTy, body) =>
      val thisTy = Type.single(FuncTy(param.typeConstraint, retTy).at(expr)).at(expr)
      for {
        unifiedTy <- unify(thisTy, expected)
        extractedTy <- unifiedTy match {
          case Type(FuncTy(refParTy, refRetTy) :: Nil, NothingTy()) => Right((refParTy, refRetTy))
          case other => Left(TypeMismatchError(expected, other))
        }
        (refParTy, refRetTy) = extractedTy
        (boundEnv, typedPat) <- bind(param, refParTy, env.enterScope())
        typedBody <- checkExpr(body, boundEnv, refRetTy)
      } yield {
        val finalFuncTy = Type.single(FuncTy(typedPat.ty, typedBody.ty).at(expr)).at(expr)
        TypedLambda(typedPat, refRetTy, typedBody, finalFuncTy)
      }

    case ExprTuple(exprs) =>
      exprs.zipWithIndex.foldLeftM((Chain.empty[TypedExpr], expected)) {
        case ((acc, curExpTy), (e, idx)) =>
          estimateLength(e, env) match {
            case Some(n) =>
              val (thisHint, nextExpTy) = sliceType(curExpTy, n)
              checkExpr(e, env, thisHint).map { tyExp => (acc :+ tyExp, nextExpTy) }
            case None =>
              checkExpr(e, env, Type.rep(UnknownTy()).at(e)).flatMap { tyExp =>
                estimateTypeLen(tyExp.ty) match {
                  case Some(n) =>
                    val (_, nextExpTy) = sliceType(curExpTy, n)
                    Right((acc :+ tyExp, nextExpTy))
                  case None =>
                    if (idx == exprs.length - 1) Right((acc :+ tyExp, Type.unit.at(e)))
                    else Left(VariadicPlacementError(tyExp.ty, e))
                }
              }
          }
      }.flatMap { case (typedExprs, _) =>
        val exprList = typedExprs.toList
        for {
          combinedType <- exprList.foldLeftM(Type.unit.at(expr): Type) { (accTy, tExpr) => concatTypes(accTy, tExpr.ty, expr) }
          finalTy <- unify(combinedType, expected)
        } yield TypedTuple(exprList, finalTy)
      }

    case ExprTagged(inner) =>
      val innerExpected = expected.prefix match {
        case TaggedTy(inTy) :: Nil if expected.rest == NothingTy() => inTy
        case _ => Type.rep(UnknownTy()).at(expr)
      }
      checkExpr(inner, env, innerExpected).map { tEx =>
        TypedTagged(tEx, Type.single(TaggedTy(tEx.ty).at(expr)).at(expr))
      }

    case Conditional(cond, thenExpr, elseExpr) =>
      for {
        typedCond <- checkExpr(cond, env, Type.single(BoolTy()).at(cond))
        typedThen <- checkExpr(thenExpr, env, expected)
        typedElse <- checkExpr(elseExpr, env, expected)
        mergedTy <- unify(typedThen.ty, typedElse.ty)
          .orElse(unify(typedElse.ty, typedThen.ty))
          .left.flatMap(_ => Left(IncompatibleBranchesError(typedThen.ty, typedElse.ty, expr)))
        finalTy <- unify(mergedTy, expected)
      } yield TypedConditional(typedCond, typedThen, typedElse, finalTy)
  }
}