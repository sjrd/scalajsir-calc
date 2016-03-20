package calc

import fastparse.core.Parsed

import org.scalajs.core.ir
import ir.Printers._

import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency

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

        case failure =>
          Console.err.println(failure)
          System.exit(2)
      }
    }
  }

  private def compileAndRun(tree: Tree): Unit = {
    val classDef = Compiler.compileMainClass(tree)

    val writer = new java.io.PrintWriter(System.out)
    try {
      val printer = new IRTreePrinter(writer)
      printer.printTopLevelTree(classDef)
    } finally {
      writer.flush()
    }

    linkAndRun(classDef)
  }

  private def linkAndRun(classDef: ir.Trees.ClassDef): Unit = {
    // Load the standard library
    val libraryName = "scalajs-library_2.11-0.6.8.jar"
    val libraryJarStream = getClass.getResourceAsStream(libraryName)
    val libraryBytes = try {
      scala.reflect.io.Streamable.bytes(libraryJarStream)
    } finally {
      libraryJarStream.close()
    }
    val libraryVirtualFile = {
      (new MemVirtualBinaryFile(libraryName) with VirtualJarFile)
        .withContent(libraryBytes)
        .withVersion(Some(libraryName)) // unique
    }
    val cache = (new IRFileCache).newCache
    val libraryIRFiles = cache.cached(List(
        IRFileCache.IRContainer.Jar(libraryVirtualFile)))

    // Put the `classDef` in a virtual file
    val mainIRFile = {
      new VirtualScalaJSIRFile {
        def path: String = "main.sjsir"
        def exists: Boolean = true

        val infoAndTree: (ir.Infos.ClassInfo, ir.Trees.ClassDef) =
          (ir.Infos.generateClassInfo(classDef), classDef)
      }
    }

    linkAndRun(libraryIRFiles :+ mainIRFile)
  }

  private def linkAndRun(irFiles: Seq[VirtualScalaJSIRFile]): Unit = {
    import org.scalajs.core.tools.linker._

    val linker = Linker()
    val logger = new ScalaConsoleLogger()

    val output = WritableMemVirtualJSFile("output.js")
    linker.link(irFiles, output, logger)

    // Clearly separate the output of the program from the compiling logs
    println("")
    println("")

    run(output)
  }

  private def run(jsFile: VirtualJSFile): Unit = {
    import org.scalajs.jsenv._

    val jsEnv = new nodejs.NodeJSEnv()
      .loadLibs(Seq(ResolvedJSDependency.minimal(jsFile)))

    val code =
      s"""console.log(${Compiler.MainObjectFullName}().main());\n"""
    val codeFile = (new MemVirtualJSFile("maincode.js"))
      .withContent(code)
      .withVersion(Some("maincode.js")) // unique

    val runner = jsEnv.jsRunner(codeFile)

    runner.run(NullLogger, ConsoleJSConsole)
  }
}
