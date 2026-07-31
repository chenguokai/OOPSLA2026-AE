def runInNewProcess(name: String): Int = {
  val javaHome = System.getProperty("java.home")
  val javaBin = s"$javaHome/bin/java"
  val classpath = System.getProperty("java.class.path")

  val processBuilder = new ProcessBuilder(
    javaBin,
    "-cp",
    classpath,
    name
  )

  val process = processBuilder.inheritIO().start()
  process.waitFor()
}

def runInNewProcessWithParam(name: String, params: String*): Int = {
  val javaHome = System.getProperty("java.home")
  val javaBin = s"$javaHome/bin/java"
  val classpath = System.getProperty("java.class.path")

  // Build the full command: java, -cp, classpath, main class, then any extra parameters.
  val command = Seq(javaBin, "-cp", classpath, name) ++ params

  val processBuilder = new ProcessBuilder(command: _*)
  val process = processBuilder.inheritIO().start()
  process.waitFor()
}

/*
object MacroExample {

  // Inline function that takes a code block (Expr[Unit]) and prints the AST of each statement.
  inline def inspectStatements(inline expr: Unit): Unit = ${ inspectStatementsImpl('expr) }

  def inspectStatementsImpl(expr: Expr[Unit])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    // Match the Expr with a block of code
    expr.asTerm match {
      case Inlined(_, _, Block(stats, _)) =>
        stats.foreach { stat =>
          println("Multiple")
          //println(s"AST Node: ${stat.show(using Printer.TreeStructure)}")
          println(s"AST Node: ${stat.show}")
        }
      case _ =>
        println("Single")
        //println(s"AST Node: ${expr.asTerm.show(using Printer.TreeStructure)}")
        println(s"AST Node: ${expr.asTerm.show}")
    }

    // Return a unit expression to complete the macro
    '{}
  }

}
*/
