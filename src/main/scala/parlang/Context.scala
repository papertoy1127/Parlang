/*
package parlang

import parlang.ast.*
import parlang.parser.*
import parlang.runtime.*

class Context(initialEnv: Map[String, Value] = StdLib.env) {
  var env: Map[String, Value] = initialEnv

  def execln(ln: String): Option[Value] = {
    val statement = Parser.apply(ln)
    val (newEnv, v) = Interp.interp(statement, env)
    env = newEnv
    v
  }

  def exec(prog: String): Option[Value] = {
    execAst(Parser.parseProgram(prog))
  }

  def execAst(ast: List[Statement]): Option[Value] = {
    ast.foldLeft[Option[Value]](None) { (_, stmt) =>
      val (newEnv, v) = Interp.interp(stmt, env)
      env = newEnv
      v
    }
  }
}
*/