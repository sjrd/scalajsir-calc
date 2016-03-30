package calc

object Typer {

  type TypeEnv = Map[Ident, Type]

  def emptyEnv = Map[Ident, Type]()

  def inferType(tree: Tree)(implicit env: TypeEnv): TreeT = {
    tree match {
      case t:Literal => implicit val pos = t.pos
        LiteralT(t.value)
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
    val body = inferType(t.body)(env = env + (t.name -> value.tpe))
    new LetT(t.name, value, body) { val tpe = body.tpe }
  }
}
