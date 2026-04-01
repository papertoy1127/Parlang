package parlang.runtime

import parlang.ast.*

object Interp {
  def error(str: String): Nothing = throw Exception(s"[Interpretation Error] $str")
  private def truth(v: Value): Option[Boolean] = v.elems match {
    case Nil => Some(false)
    case ValueBool(b) :: Nil => Some(b)
    case ValueNum(x) :: Nil => Some(x != 0)
    case ValueChar(x) :: Nil => Some(x != '\u0000')
    case ValueString(x) :: Nil => Some(x.nonEmpty)
    case _ => None
  }

  def bind(binder: Pattern, value: Value, env: Map[String, Value]): Map[String, Value] = binder match {
    case ExactPattern(elems) =>
      if (elems.length != value.elems.length) error("Unmatched pattern length")
      elems.zip(value.elems).foldLeft(env) { case (acc, (pat, vat)) => bindAtom(pat, vat, acc) }

    case RestPattern(elems, rest) =>
      if (value.elems.length < elems.length) error("Not enough elements to unpack")
      val (prefixVals, restVals) = value.elems.splitAt(elems.length)
      val prefixEnv = elems.zip(prefixVals).foldLeft(env) { case (acc, (pat, vat)) => bindAtom(pat, vat, acc) }
      rest match {
        case DiscardPat(_) => prefixEnv
        case IdPat(id, _) => prefixEnv + (id -> Value(restVals))
      }
  }

  private def bindAtom(pat: PatAtom, vat: ValAtom, env: Map[String, Value]): Map[String, Value] = (pat, vat) match {
    case (IdPat(id, _), _) => env + (id -> Value.singular(vat))
    case (DiscardPat(_), _) => env
    case (TaggedPat(innerPat), ValueTagged(innerVal)) => bind(innerPat, innerVal, env)
    case _ => error(s"Unmatched atom: expected $pat but got $vat")
  }

  def interp(stmt: Statement, env: Map[String, Value]): (Map[String, Value], Option[Value]) = stmt match {
    case expr: Expression => (env, Some(evaluate(expr, env)))
    case Variable(binder, expr) => (bind(binder, evaluate(expr, env), env), None)
    case FuncRec(id, params, _, body) =>
      val curry = params.tail.foldRight(body) { (param, inner) => Lambda(param, None, inner) }
      val closure = ValueRecClosure(id, params.head, curry, env)
      (env + (id -> Value.singular(closure)), None)
  }

  private def isEqual(v1: Value, v2: Value): Boolean = {
    if (v1.elems.length != v2.elems.length) return false
    v1.elems.zip(v2.elems).foldLeft(true) {
      case (false, _) => false
      case (true, (x, y)) => (x, y) match {
        case (_: CallableAtom, _) | (_, _: CallableAtom) =>
          error("Cannot compare functions for equality")

        case _ => x == y
      }
    }
  }

  private def evaluate(expr: Expression, env: Map[String, Value]): Value = expr match {
    case Literal(v) => v

    case OperatorUnary(op, operand) =>
      val v = evaluate(operand, env)
      (op, v.elems) match {
        case (UnaryOperType.Plus, ValueNum(n) :: Nil)  => Value.singular(ValueNum(n))
        case (UnaryOperType.Minus, ValueNum(n) :: Nil) => Value.singular(ValueNum(n))
        case (UnaryOperType.LogicalNot, _) => truth(v) match {
          case Some(b) => Value.singular(ValueBool(!b))
          case None    => error(s"LogicalNot failed: $v is not a logical value")
        }
        case _ => error(s"Type mismatch in unary operation: $op $v")
      }

    case OperatorBinary(BinaryOperType.LogicalAnd, l, r) =>
      truth(evaluate(l, env)) match {
        case Some(true) => truth(evaluate(r, env)) match {
          case Some(br) => Value.singular(ValueBool(br))
          case _ => error(s"LogicalAnd right operand is not logical: $r")
        }
        case Some(false) => Value.singular(ValueBool(false))
        case _ => error(s"LogicalAnd left operand is not logical: $l")
      }


    case OperatorBinary(BinaryOperType.LogicalOr, l, r) =>
      truth(evaluate(l, env)) match {
        case Some(true) => Value.singular(ValueBool(true))
        case Some(false) => truth(evaluate(r, env)) match {
          case Some(br) => Value.singular(ValueBool(br))
          case _ => error(s"LogicalOr right operand is not logical: $r")
        }
        case _ => error(s"LogicalOr left operand is not logical: $l")
      }

    case OperatorBinary(op, left, right) =>
      val (l, r) = (evaluate(left, env), evaluate(right, env))
      (op, l.elems, r.elems) match {
        case (BinaryOperType.Plus, ValueNum(x) :: Nil, ValueNum(y) :: Nil)  => Value.singular(ValueNum(x + y))
        case (BinaryOperType.Minus, ValueNum(x) :: Nil, ValueNum(y) :: Nil) => Value.singular(ValueNum(x - y))
        case (BinaryOperType.Times, ValueNum(x) :: Nil, ValueNum(y) :: Nil) => Value.singular(ValueNum(x * y))
        case (BinaryOperType.Over, ValueNum(x) :: Nil, ValueNum(y) :: Nil) =>
          if (y == 0) error("Division by zero")
          else Value.singular(ValueNum(x / y))

        case (BinaryOperType.Equals, _, _) => Value.singular(ValueBool(isEqual(l, r)))
        case (BinaryOperType.NotEquals, _, _) => Value.singular(ValueBool(!isEqual(l, r)))

        case _ => error(s"Type mismatch in binary operation: $l $op $r")
      }

    case Identifier(id) => env.getOrElse(id, error(s"Free identifier: $id"))

    case Scope(stmts) =>
      val (_, res) = stmts.foldLeft((env, None: Option[Value])) {
        case ((currentEnv, _), stmt) => Interp.interp(stmt, currentEnv)
      }

      res.getOrElse(Value.Unit)
    case Lambda(param, _, expr) => Value.singular(ValueClosure(param, expr, env))
    case Call(expr, arg) => evaluate(expr, env).elems match {
      case ValueClosure(param, body, closureEnv) :: Nil => evaluate(body, bind(param, evaluate(arg, env), closureEnv))
      case (rec @ ValueRecClosure(id, param, body, closureEnv)) :: Nil => evaluate(body, bind(param, evaluate(arg, env), closureEnv) + (id -> Value.singular(rec)))
      case ValueBuiltin(name, func) :: Nil => func(evaluate(arg, env))
      case ValQuit :: Nil => Value.singular(ValQuit)
      case _ => error(s"Cannot call: $expr")
    }

    case Conditional(cond, thenExpr, elseExpr) => truth(evaluate(cond, env)) match {
      case Some(true) => evaluate(thenExpr, env)
      case Some(false) => evaluate(elseExpr, env)
      case _ => error(s"Conditional $cond is not logical")
    }

    case ExprTagged(expr) => Value.singular(ValueTagged(evaluate(expr, env)))
    case ExprTuple(exprs) => Value(exprs.flatMap(evaluate(_, env).elems))
  }
}

