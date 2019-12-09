package calc

import java.nio.file.Path

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import fastparse.Parsed

import org.scalajs.ir
import org.scalajs.ir.Printers._

import org.scalajs.logging._
import org.scalajs.jsenv._

/** Main object of the calculator.
 *
 *  You do not need to modify this object at all. Your work is in
 *  `Compiler.scala`.
 *
 *  It might still be interesting to poke in this code for debugging purposes.
 *  By default, it will print the text-based representation of the output of
 *  your compiler, then link the problem and execute it.
 */
object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      Console.err.println("""Usage: calc.Main "<expr>"""")
      System.exit(1)
    } else {
      val code = args(0)

      Parser.parse(code) match {
        case Parsed.Success(tree, _) =>
          compileAndRun(tree)

        case Parsed.Failure(lp, index, _) =>
          Console.err.println(code)
          Console.err.println(" " * index + "^")
          Console.err.println(s"parse error (expected: $lp)")
          System.exit(2)
      }
    }
  }

  private def compileAndRun(tree: Tree): Unit = {
    val classDef = Compiler.compileMainClass(tree)

    val writer = new java.io.PrintWriter(System.out)
    try {
      val printer = new IRTreePrinter(writer)
      printer.print(classDef)
      writer.println()
    } finally {
      writer.flush()
    }

    val linked = Linker.link(classDef, new ScalaConsoleLogger(Level.Info))

    // Clearly separate the output of the program from the compiling logs
    println()
    println()

    run(linked)
  }

  private def run(jsFile: Path): Unit = {
    val jsEnv = new nodejs.NodeJSEnv()
    val input = Seq(Input.Script(jsFile))
    val runConfig = RunConfig().withLogger(NullLogger)
    Await.result(jsEnv.start(input, runConfig).future, Duration.Inf)
  }

}
