package parlang

@main def main(): Unit = {
  val ctx = Context()
  Repl.run(ctx)
}

