package calc.stdlib

import calc.{FunctionType, TreeType}

/** Some simple mathematical functions to use with the language. */
object Math {

  val moduleName: String = "jl_Math$"

  val functionTypes: Map[String, TreeType] = Map(
    "abs"   -> FunctionType(1),
    "acos"  -> FunctionType(1),
    "asin"  -> FunctionType(1),
    "atan"  -> FunctionType(1),
    "ceil"  -> FunctionType(1),
    "cos"   -> FunctionType(1),
    "cosh"  -> FunctionType(1),
    "exp"   -> FunctionType(1),
    "floor" -> FunctionType(1),
    "log"   -> FunctionType(1),
    "round" -> FunctionType(1),
    "sin"   -> FunctionType(1),
    "sinh"  -> FunctionType(1),
    "tan"   -> FunctionType(1),
    "tanh"  -> FunctionType(1)
  )
}
