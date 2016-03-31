package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}

object Foreign {

  // Return a type environment extension from foreign functions information
  def staticJavaMethods(clsName: String, importedMethods: List[(String, TFun)]): Typer.TypeEnv = {
    importedMethods.foldLeft(Typer.emptyEnv) { (env, value) =>
      val (name, tpe) = value
      val mangled = name + ("__D" * (tpe.arity + 1))
      val foreignTpe = TStaticForeignFun(clsName, mangled, tpe.arity)
      env + (name -> foreignTpe)
    }
  }

  // Returns a new IR tree prepended with eta expansion for all functions defined in typeEnv
  def prependEtaExpansion(ir: irt.Tree, typeEnv: Typer.TypeEnv): irt.Tree = {
    implicit val env = typeEnv
    implicit val pos = ir.pos
    val definitions: List[irt.Tree] = typeEnv.foldRight(List.empty[irt.Tree]) { (kv, acc) =>
      val (name, tpe) = kv
      tpe match {
        case t:TStaticForeignFun => {
          val params = (1 to t.arity).map({ n => Ident("a__" + n) }).toList
          val closure = Closure(params, Call(Ident(name), params))
          val typedClosure = Typer.closure(closure)
          val compiledClosure = Compiler.closure(typedClosure)
          val definition = irt.VarDef(irt.Ident(name), irtpe.AnyType, false, compiledClosure)
          definition :: acc
        }
        case _ => acc
      }
    }
    irt.Block(definitions ++ List(ir))
  }
}
