package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}

sealed trait ForeignImport
case class StaticForeignFunction(clsName: String, methodName: String, tpe: TFun) extends ForeignImport

object Foreign {
  type Env = Map[String, ForeignImport]

  def emptyEnv = Map.empty[String, ForeignImport]

  // Return a pair of type and foreign function environments by importing static methods
  def staticJavaMethods(clsName: String, importedMethods: List[(String, TFun)]): (Typer.TypeEnv, Env) = {
    importedMethods.foldLeft(Typer.emptyEnv, emptyEnv) { (env, value) =>
      val (typeEnv, foreignEnv) = env
      val (name, tpe) = value
      val mangled = name + ("__D" * (tpe.arity + 1))
      val foreignTpe = StaticForeignFunction(clsName, mangled, tpe)
      (typeEnv + (name -> tpe), foreignEnv + (name -> foreignTpe))
    }
  }
}
