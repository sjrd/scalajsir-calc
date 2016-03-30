package calc

import org.scalajs.core.ir.{Trees => irt}

object NodeCompiler {
  def apply(f: Tree => irt.Tree): NodeCompiler = {
    new NodeCompiler {
      def apply(tree: Tree): irt.Tree = {
        f(tree)
      }
    }
  }
}

trait NodeCompiler extends (Tree => irt.Tree) {
  def apply(tree: Tree): irt.Tree

  def |(other: NodeCompiler): NodeCompiler = {
    val that = this
    return new NodeCompiler {
      def apply(tree: Tree): irt.Tree = {
        try {
          that(tree)
        } catch {
          case e: CompileError => other(tree)
          case other: Throwable => throw other
        }
      }
    }
  }
}