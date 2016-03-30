package calc

object Typer {

  type TypeEnv = Map[String, Type]

  def emptyEnv = Map[String, Type]()

  def inferType(tree: Tree)(implicit env: TypeEnv): TreeT = {
    implicit val pos = tree.pos
    tree match {
      case t:Ident =>
        val result = env.get(t.name) getOrElse (throw new UnboundVariable(t))
        new IdentT(t.name) { val tpe = result }
      case t:Literal => LiteralT(t.value)
      case t:BinaryOp => binaryOp(t)
      case t:Let => letBinding(t)
    }
  }

  def binaryOp(t: BinaryOp)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val lhs = inferType(t.lhs)
    val rhs = inferType(t.rhs)
    if (lhs.tpe == TDouble && lhs.tpe == rhs.tpe) {
      new BinaryOpT(t.op, lhs, rhs) { val tpe = lhs.tpe }
    } else {
      throw new TypeError(rhs.pos, lhs.tpe, rhs.tpe)
    }
  }

  def letBinding(t: Let)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val value = inferType(t.value)
    val body = inferType(t.body)(env = env + (t.name.name -> value.tpe))
    new LetT(t.name, value, body) { val tpe = body.tpe }
  }
}
