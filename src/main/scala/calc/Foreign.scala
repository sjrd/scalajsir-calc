package calc

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
}
