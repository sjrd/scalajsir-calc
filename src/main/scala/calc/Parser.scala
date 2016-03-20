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
  // TODO Provide meaningful positions for the trees we generate
  implicit val DummyPos = Position.NoPosition

  val White = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }

  import fastparse.noApi._
  import White._

  def parse(code: String): Parsed[Tree] = {
    expr.parse(code)
  }

  private val number: P[Tree] = P( CharIn('0' to '9').rep(1).!.map(_.toInt) ).map(Literal(_))

  private val parens: P[Tree] = P( "(" ~/ addSub ~ ")" )

  private val factor: P[Tree] = P( number | parens )

  private val divMul: P[Tree] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map {
    case (leftMost, ops) =>
      (leftMost /: ops) { case (lhs, (op, rhs)) =>
        BinaryOp(op, lhs, rhs)
      }
  }

  private val addSub: P[Tree] = P( divMul ~ (CharIn("+-").! ~/ divMul).rep ).map {
    case (leftMost, ops) =>
      (leftMost /: ops) { case (lhs, (op, rhs)) =>
        BinaryOp(op, lhs, rhs)
      }
  }


  private val expr: P[Tree] = P( addSub ~ End )
}
