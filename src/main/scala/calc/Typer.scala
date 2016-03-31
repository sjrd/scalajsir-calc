package calc

object Typer {

  type TypeEnv = Map[String, Type]

  def emptyEnv = Map[String, Type]()

  def inferType(tree: Tree)(implicit env: TypeEnv): TreeT = {
    implicit val pos = tree.pos
    tree match {
      case t:Ident =>
        val result = env.getOrElse(t.name, throw new UnboundVariable(t))
        new IdentT(t.name) { val tpe = result }
      case t:Literal => LiteralT(t.value)
      case t:BinaryOp => binaryOp(t)
      case t:Let => letBinding(t)
      case t:If => ifElse(t)
      case t:Closure => closure(t)
      case t:Call => call(t)
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
        new IfT(cond, thenp, elsep) {
          val tpe = thenp.tpe
        }
      }
    }
  }

  def closure(t: Closure)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val params = t.params map { p => new IdentT(p.name) { val tpe = TDouble } }
    val body = inferType(t.body)(env ++ (t.params map { _.name -> TDouble }))
    new ClosureT(params, body) { val tpe = TFun(params.length) }
  }

  def call(t: Call)(implicit env: TypeEnv): TreeT = { implicit val pos = t.pos
    val funType = inferType(t.fun)

    // All arguments must be double
    val typedArgs = t.args map { t =>
      val typed = inferType(t)
      if (typed.tpe == TDouble) {
        typed
      } else {
        throw new TypeError(t.pos, TDouble, typed.tpe)
      }
    }

    funType.tpe match {
      case TFun(arity)  =>
        val calleeArity = t.args.length
        // Quick exit if parameter length does not match
        if (arity != calleeArity) {
          throw new InvalidNumberOfArgument(pos, arity, calleeArity)
        } else {
          new CallT(funType, typedArgs) { val tpe = TDouble }
        }
      case TStaticForeignFun(clsName, methodName, arity) =>
        val calleeArity = t.args.length
        // Quick exit if parameter length does not match
        if (arity != calleeArity) {
          throw new InvalidNumberOfArgument(pos, arity, calleeArity)
        } else {
          new ForeignCallT(clsName, methodName, typedArgs) { val tpe = TDouble }
        }
      case other =>
        throw new TypeError(funType.pos, TFun(t.args.length), other)
    }
  }
}
