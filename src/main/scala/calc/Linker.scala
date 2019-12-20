package calc

import java.nio.file.Path

import scala.concurrent._
import scala.concurrent.duration.Duration

import com.google.common.jimfs.Jimfs

import org.scalajs.ir
import org.scalajs.ir.Printers._
import org.scalajs.ir.Trees.ClassDef

import org.scalajs.logging._

import org.scalajs.linker.interface._
import org.scalajs.linker._
import org.scalajs.linker.interface.unstable.IRFileImpl

/** The linker, to produce something runnable from the output of the compiler.
 *
 *  You do not need to modify this object.
 */
object Linker {

  import scala.concurrent.ExecutionContext.Implicits.global

  private lazy val libraryIRFiles = {
    // Load the standard library
    val libraryPathStr = System.getProperty("calc.scalajslib")
    val libraryPath = new java.io.File(libraryPathStr).toPath()
    val cache = StandardImpl.irFileCache().newCache
    val future = PathIRContainer.fromClasspath(Seq(libraryPath)).flatMap {
      pair => cache.cached(pair._1)
    }
    Await.result(future, Duration.Inf)
  }

  def link(classDef: ClassDef, logger: Logger): Path = {
    val mainIRFile = new ClassDefIRFileImpl("main.sjsir", None, classDef)
    val allIRFiles = libraryIRFiles :+ mainIRFile
    val config = StandardConfig().withCheckIR(true)
    val linker = StandardImpl.linker(config)

    val output = Jimfs.newFileSystem().getPath("output.js")
    val future = linker.link(allIRFiles,
        List(ModuleInitializer.mainMethod(Compiler.MainClassFullName, "main")),
        LinkerOutput(PathOutputFile(output)), logger)
    Await.result(future, Duration.Inf)
    output
  }

  /** A simple in-memory IR file containing a `ClassDef`. */
  private final class ClassDefIRFileImpl(
      path: String,
      version: Option[String],
      classDef: ClassDef
  ) extends IRFileImpl(path, version) {
    def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] =
      Future.successful(ir.EntryPointsInfo.forClassDef(classDef))

    def tree(implicit ec: ExecutionContext): Future[ClassDef] =
      Future.successful(classDef)
  }

}
