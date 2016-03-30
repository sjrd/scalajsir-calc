package calc

class CompileErrorException(cause: String) extends Exception(cause)

case class TypeCheckException(found: Type, expected: Type)
  extends CompileErrorException(
    s"Type error (expected ${expected}, found ${found})"
)

case class UnknownIdentException(name: String)
  extends CompileErrorException(s"Identifier ${name} not defined")
