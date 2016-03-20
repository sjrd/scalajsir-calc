package calc

import org.scalajs.core.ir.Position

/** The parser.
 *
 *  You do not need to modify this file, but you can if you want to extend the
 *  "language" you're compiling.
 *
 *  This is written using FastParse:
 *  http://www.lihaoyi.com/fastparse/
 */
object Parser {
  def parse(code: String): fastparse.core.Parsed[Tree] = {
    val sourceFile = new java.net.URI("virtualfile://sourcecode.txt")
    def indexToPos(index: Int): Position = {
      Position(sourceFile, line = 0, column = index)
    }

    new Syntactic(indexToPos _).program.parse(code)
  }

  // Lexical analysis (for which whitespace is significant)
  private class Lexical(indexToPosition: Int => Position) {
    import fastparse.all._
    private implicit val indexToPositionView = indexToPosition

    // Literals
    private val sign = CharIn("+-")
    private val digit10 = CharIn("0123456789")
    private def isStringChar(c: Char): Boolean = (c != '\n' && c != '"')
    private val unicodeChar =
      (CharPred(!Character.isHighSurrogate(_))
         | (CharPred(Character.isHighSurrogate)
              ~ CharPred(Character.isLowSurrogate)))

    private val number = P(Index ~ (sign.? ~ digit10 ~/ digit10.rep ~ ("." ~/ digit10.rep(1)).?).!)
      .map { case (i, s) => Literal(s.toDouble)(i) }

    val literal = P(number)

    // Identifiers
    private val identStart = CharPred(Character.isJavaIdentifierStart(_))
    private val identCont = CharPred(Character.isJavaIdentifierPart(_))

    val identStr = (identStart ~/ identCont.rep).!
    val identifier = (Index ~ identStr).map { case (i, n) => Ident(n)(i) }

    // Keywords
    private def kw(n: String): Parser[Unit] =
      n ~ !identCont

    val kLet = kw("let")
    val kIn = kw("in")
    val kIf = kw("if")
    val kElse = kw("else")
    val kFun = kw("fun")
  }

  // Syntactic analysis (for which whitespace and comments are ignored)
  private class Syntactic(indexToPosition: Int => Position) {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      (CharIn(" \t\n\r")
         | (";" ~ CharPred(c => c != '\n' && c != '\r').rep)).rep
    }
    import White._
    import fastparse.noApi._

    val lexer = new Lexical(indexToPosition)
    import lexer._

    private implicit val indexToPositionView = indexToPosition

    val program: Parser[Tree] =
      P("" ~ expr ~ End) // The initial "" allows leading whitespace

    private val expr = P(ifThenElse | let | fun | addSub)

    private val parens: P[Tree] = P("(" ~/ addSub ~ ")")

    private val base: P[Tree] = P(
        literal | identifier | parens
    )

    private val factor: P[Tree] = P(base ~ (Index ~ "(" ~/ args ~ ")").rep).map {
      case (baseValue, applications) =>
        (baseValue /: applications) { case (fun, (i, args)) =>
          Call(fun, args)(i)
        }
    }

    private val args: P[List[Tree]] = P(expr.rep(0, ",")).map(_.toList)

    private val let: P[Tree] = P(Index ~ kLet ~/ identifier ~ "=" ~ expr ~ kIn ~/ expr).map {
      case (i, name, value, body) =>
        Let(name, value, body)(i)
    }

    private val fun: P[Tree] = P(Index ~ kFun ~/ "(" ~ identifier.rep(0, ",") ~ ")" ~ "=" ~ "{" ~ expr ~ "}").map {
      case (i, params, body) =>
        Closure(params.toList, body)(i)
    }

    private val call: P[Tree] = P(Index ~ identifier ~ "(" ~/ expr.rep(0, ",")).map {
      case (i, fun, args) =>
        Call(fun, args.toList)(i)
    }

    private val divMul: P[Tree] = P(factor ~ (Index ~ CharIn("*/").! ~/ factor).rep).map {
      case (leftMost, ops) =>
        (leftMost /: ops) { case (lhs, (i, op, rhs)) =>
          BinaryOp(op, lhs, rhs)(i)
        }
    }

    private val addSub: P[Tree] = P(divMul ~ (Index ~ CharIn("+-").! ~/ divMul).rep).map {
      case (leftMost, ops) =>
        (leftMost /: ops) { case (lhs, (i, op, rhs)) =>
          BinaryOp(op, lhs, rhs)(i)
        }
    }

    private val ifThenElse: P[Tree] = P(Index ~ "if" ~/ "(" ~ expr ~ ")" ~ addSub ~ "else" ~ addSub).map {
      case (i, cond, thenp, elsep) =>
        If(cond, thenp, elsep)(i)
    }

  }
}
