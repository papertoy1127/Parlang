package parlang

import scala.io.StdIn
import parlang.parser.Parser
import parlang.parser.Parser.{ParseResult, ParseState}
import parlang.runtime.{Interp, ValQuit, Value}

object Repl {
  def run(ctx: Context): Unit = {
    println("Welcome to the PARLang REPL!")
    println("Type \u001b[36m:q\u001b[0m or \u001b[36mquit\u001b[0m to quit")

    val buffer = new StringBuilder()

    while (true) {
      print(if (buffer.isEmpty) ">> " else ".. ")
      val line = StdIn.readLine()

      if (line == null || (buffer.isEmpty && line.trim == ":q")) return

      buffer.append(line).append("\n")
      val code = buffer.toString()

      if (code.trim.isEmpty) buffer.clear()
      else {
        Parser.parseRepl(code) match {
          case ParseState.Complete(ast) =>
            try {
              ctx.exec(code) match {
                case Some(Value(ValQuit :: Nil)) => return
                case Some(value) => println(s"\u001b[32m=> $value\u001b[0m")
                case None => ()
              }
            } catch case e: Exception => println(s"\u001b[31m${e.getMessage}\u001b[0m")

            buffer.clear()

          case ParseState.Incomplete => ()
          case ParseState.Error(msg) =>
            println(s"\u001b[31m[Parsing Error] $msg\u001b[0m")
            buffer.clear()
        }
      }
    }
  }
}