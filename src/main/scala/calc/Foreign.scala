package calc

object Foreign {

  // Return a type environment extension from foreign functions information
  def staticJavaMethods(clsName: String, importedMethods: Map[String, (String, TFun)]): Typer.TypeEnv = {
    importedMethods.foldLeft(Typer.emptyEnv) { (env, value) =>
      val (actualName, (renamedTo, tpe)) = value
      val foreignTpe = TStaticForeignFun(clsName, actualName, tpe.arity)
      env + (renamedTo -> foreignTpe)
    }
  }
}
