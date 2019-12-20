package calc

import org.scalajs.ir.Position

import fastparse._

/** The parser.
 *
 *  You do not need to modify this file, but you can if you want to extend the
 *  "language" you're compiling.
 *
 *  This is written using FastParse:
 *  http://www.lihaoyi.com/fastparse/
 */
object Parser {
  def parse(code: String): fastparse.Parsed[Tree] = {
    val sourceFile = new java.net.URI("virtualfile://sourcecode.txt")
    def indexToPos(index: Int): Position = {
      Position(sourceFile, line = 0, column = index)
    }

    fastparse.parse(code, x => new Syntactic(indexToPos(_)).program(x))
  }

  // Lexical analysis (for which whitespace is significant)
  private class Lexical(indexToPosition: Int => Position) {
    import fastparse.NoWhitespace._

    private implicit val indexToPositionView = indexToPosition

    // Literals
    private def sign[_: P] = CharIn("+\\-")
    private def digit10[_: P] = CharIn("0123456789")
    private def isStringChar(c: Char): Boolean = (c != '\n' && c != '"')
    private def unicodeChar[_: P] =
      (CharPred(!Character.isHighSurrogate(_))
         | (CharPred(Character.isHighSurrogate)
              ~ CharPred(Character.isLowSurrogate)))

    private def number[_: P] = P(Index ~ (sign.? ~ digit10 ~/ digit10.rep ~ ("." ~/ digit10.rep(1)).?).!)
      .map { case (i, s) => Literal(s.toDouble)(i) }

    def literal[_: P] = P(number)

    // Identifiers
    private def identStart[_: P] = CharPred(Character.isJavaIdentifierStart(_))
    private def identCont[_: P] = CharPred(Character.isJavaIdentifierPart(_))

    def identStr[_: P] = (identStart ~/ identCont.rep).!
    def identifier[_: P] = (Index ~ identStr).map { case (i, n) => Ident(n)(i) }

    // Keywords
    private def kw[_: P](n: String): P[Unit] =
      n ~ !identCont

    def kLet[_: P] = kw("let")
    def kIn[_: P] = kw("in")
    def kIf[_: P] = kw("if")
    def kElse[_: P] = kw("else")
    def kFun[_: P] = kw("fun")
  }

  // Syntactic analysis (for which whitespace and comments are ignored)
  private class Syntactic(indexToPosition: Int => Position) {
    import SingleLineWhitespace._

    val lexer = new Lexical(indexToPosition)
    import lexer._

    private implicit val indexToPositionView = indexToPosition

    def program[_: P]: P[Tree] =
      P("" ~ expr ~ End) // The initial "" allows leading whitespace

    private def expr[_: P] = P(ifThenElse | let | fun | addSub)

    private def parens[_: P]: P[Tree] = P("(" ~/ expr ~ ")")

    private def base[_: P]: P[Tree] = P(
        literal | identifier | parens
    )

    private def factor[_: P]: P[Tree] = P(base ~ (Index ~ "(" ~/ args ~ ")").rep).map {
      case (baseValue, applications) =>
        applications.foldLeft(baseValue) { case (fun, (i, args)) =>
          Call(fun, args)(i)
        }
    }

    private def args[_: P]: P[List[Tree]] = P(expr.rep(0, ",")).map(_.toList)

    private def let[_: P]: P[Tree] = P(Index ~ kLet ~/ identifier ~ "=" ~ expr ~ kIn ~/ expr).map {
      case (i, name, value, body) =>
        Let(name, value, body)(i)
    }

    private def fun[_: P]: P[Tree] = P(Index ~ kFun ~/ "(" ~ identifier.rep(0, ",") ~ ")" ~ "=" ~ "{" ~ expr ~ "}").map {
      case (i, params, body) =>
        Closure(params.toList, body)(i)
    }

    private def call[_: P]: P[Tree] = P(Index ~ identifier ~ "(" ~/ expr.rep(0, ",")).map {
      case (i, fun, args) =>
        Call(fun, args.toList)(i)
    }

    private def divMul[_: P]: P[Tree] = P(factor ~ (Index ~ CharIn("*/").! ~/ factor).rep).map {
      case (leftMost, ops) =>
        ops.foldLeft(leftMost) { case (lhs, (i, op, rhs)) =>
          BinaryOp(op, lhs, rhs)(i)
        }
    }

    private def addSub[_: P]: P[Tree] = P(divMul ~ (Index ~ CharIn("+\\-").! ~/ divMul).rep).map {
      case (leftMost, ops) =>
        ops.foldLeft(leftMost) { case (lhs, (i, op, rhs)) =>
          BinaryOp(op, lhs, rhs)(i)
        }
    }

    private def ifThenElse[_: P]: P[Tree] = P(Index ~ "if" ~/ "(" ~ expr ~ ")" ~ addSub ~ "else" ~ addSub).map {
      case (i, cond, thenp, elsep) =>
        If(cond, thenp, elsep)(i)
    }

  }
}
