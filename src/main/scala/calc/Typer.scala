package calc

object Typer {

  type TypeEnv = Map[String, Type]

  def emptyEnv = Map[String, Type]()

  def inferType(tree: Tree)(implicit env: TypeEnv): TreeT = {
    implicit val pos = tree.pos
    tree match {
      case t:Ident => ident(t)
      case t:Literal => LiteralT(t.value)
      case t:BinaryOp => binaryOp(t)
      case t:Let => letBinding(t)
      case t:If => ifElse(t)
      case t:Closure => closure(t)
      case t:Call => call(t)
    }
  }

  def ident(t: Ident)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val result = env.getOrElse(t.name, throw new UnboundVariable(t))
    new IdentT(t.name) { val tpe = result }
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
    t.value match {
      case v:Closure =>
        val newEnv = env + (t.name.name -> TFun(v.params.size))
        val value = inferType(v)(newEnv)
        val body = inferType(t.body)(newEnv)
        new LetT(t.name, value, body) { val tpe = body.tpe }
      case other =>
        val value = inferType(t.value)
        val body = inferType(t.body)(env + (t.name.name -> value.tpe))
        new LetT(t.name, value, body) { val tpe = body.tpe }
    }
  }

  def ifElse(t: If)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val cond = inferType(t.cond)
    if (cond.tpe != TDouble) {
      throw new TypeError(cond.pos, TDouble, cond.tpe)
    } else {
      val thenp = inferType(t.thenp)
      val elsep = inferType(t.elsep)
      if (thenp.tpe != elsep.tpe) {
        throw new TypeError(elsep.pos, thenp.tpe, elsep.tpe)
      } else {
        new IfT(cond, thenp, elsep) { val tpe = thenp.tpe }
      }
    }
  }

  def closure(t: Closure)(implicit env: TypeEnv) = { implicit val pos = t.pos
    // Ensure all params are unique
    val paramSet = t.params.map({ _.name }).toSet
    paramSet foreach { p =>
      if (t.params.count({ _.name == p }) > 1) {
        throw new ParameterRedeclaration(pos, p)
      }
    }

    val params = t.params map { p => new IdentT(p.name) { val tpe = TDouble } }
    val body= inferType(t.body)(env ++ (t.params map { _.name -> TDouble }))
    if (body.tpe == TDouble) {
      new ClosureT(params, body) { val tpe = TFun(params.length) }
    } else {
      throw new TypeError(body.pos, TDouble, body.tpe)
    }
  }

  def call(t: Call)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val funType = inferType(t.fun)

    // All arguments must be double
    val typedArgs = t.args map inferType
    typedArgs foreach { arg =>
      if (arg.tpe != TDouble) {
        throw new TypeError(arg.pos, TDouble, arg.tpe)
      }
    }

    funType.tpe match {
      case TFun(arity) =>
        val calleeArity = t.args.length
        // Quick exit if parameter length does not match
        if (arity != calleeArity) {
          throw new InvalidNumberOfArgument(pos, arity, calleeArity)
        } else {
          new CallT(funType, typedArgs) { val tpe = TDouble }
        }
      case other =>
        throw new TypeError(funType.pos, TFun(t.args.length), other)
    }
  }
}
