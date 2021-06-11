package parser

import fastparse.JavaWhitespace._
import fastparse._
import ast.Expression._
import ast.Statement._
import ast._

/**
  * Dzięki użyciu biblioteki "FastParse" pisanie parsera wygląda podobnie do tworzenia gramatyki.
  * Każda nowa metoda jest oddzielną produkcją gramatyki.
  *
  * Składnia [_:P] bierze się z funkcjonalności parsera FastParse.
  * */
object parser {

  def parseProgram(input: String): Parsed[AST] =
    fastparse.parse(input, program(_))

  def program[_: P]: P[AST] =
    P(newLine.rep ~ line.rep(min = 1, sep = newLine.rep) ~ newLine.rep)

  def line[_: P]: P[Line] = P(statement | (expression ~ ";"))

  def statement[_: P]: P[Statement] =
    P(
      ret | ifStmt | whileStmt | forStmt | funDef | (declaration ~ ";") | (assignment ~ ";")
    )

  def declaration[_: P]: P[Declaration] =
    P((nameSegment ~ ":" ~ name) map {
      case (name, ty) => Declaration(name, ty)
    })

  def assignment[_: P]: P[Assignment] =
    P((name ~ "=" ~ expression) map {
      case (name, expr) => Assignment(name, expr)
    })

  def block[_: P]: P[Block] = P(("{" ~ program ~ "}") map Block)

  def newLine[_: P]: P[Unit] = "\n" | "\r\n"

  def ifStmt[_: P]: P[IfStmt] =
    P(("if" ~ expression ~ block ~ ("else" ~ block).?) map {
      case (expr, ifBlock, elseBlock) => IfStmt(expr, ifBlock, elseBlock)
    })

  def whileStmt[_: P]: P[WhileStmt] =
    P(("while" ~ expression ~ block) map {
      case (expr, block) => WhileStmt(expr, block)
    })

  def forStmt[_: P]: P[ForStmt] =
    P(("for" ~ assignment.? ~ ";" ~ expression.? ~ ";" ~ assignment.? ~ block) map {
      case (init, cond, end, block) => ForStmt(init, cond, end, block)
    })

  def funCall[_: P]: P[FunCall] =
    P((name ~ "(" ~ expression.rep(sep = ",") ~ ")") map {
      case (identifier, args) => FunCall(identifier, args)
    })

  def funDef[_: P]: P[Statement] =
    P(
      ("fn" ~ name ~ "(" ~ varDecl
        .rep(sep = ",") ~ ")" ~ (":" ~ name).? ~ block) map {
        case (identifier, args, retTy, code) =>
          FunDef(identifier, args, retTy, code)
      }
    )

  def ret[_: P]: P[Return] = P(("return" ~ expression ~ ";") map Return)

  def varDecl[_: P]: P[Declaration] =
    P((nameSegment ~ ":" ~ name) map {
      case (name, ty) => Declaration(name, ty)
    })

  def name[_: P]: P[Name] = P(nameSegment.rep(min = 1, sep = ".") map Name)

  def nameSegment[_: P]: P[NameSegment] =
    P((CharIn("_a-zA-Z") ~ CharIn("_a-zA-Z0-9").rep).! map NameSegment)

  def literal[_: P]: P[Expression] = P(intLit | strLit | boolLit)

  def intLit[_: P]: P[IntConstant] =
    P(("-".!.? ~ CharIn("0-9") ~ CharIn("0-9").rep).! map { str =>
      IntConstant(str.toInt)
    })

  def strLit[_: P]: P[StrConstant] =
    P(("\"" ~~ CharsWhile(_ != '"', 0).! ~~ "\"") map StrConstant)

  def boolLit[_: P]: P[BoolConstant] =
    P(("true" | "false").! ~ (!nameSegment) map { x =>
      println(x)
      x match {
        case "true"  => BoolConstant(true)
        case "false" => BoolConstant(false)
      }
    })

  def expression[_: P]: P[Expression] = P(
    "(" ~/ expression ~ ")"
      | exprChain(comparison, ("&&" | "||").! map {
        case "&&" => Operator.And
        case "||" => Operator.Or
      })
  )

  def comparison[_: P]: P[Expression] =
    exprChain(addition, ("==" | "!=" | ">=" | "<=" | ">" | "<").! map {
      case "==" => Operator.Eq
      case "!=" => Operator.NEq
      case ">=" => Operator.GE
      case "<=" => Operator.LE
      case ">"  => Operator.GT
      case "<"  => Operator.LT
    })

  def addition[_: P]: P[Expression] =
    exprChain(multiplication, ("+" | "-").! map {
      case "+" => Operator.Plus
      case "-" => Operator.Minus
    })

  def multiplication[_: P]: P[Expression] =
    exprChain(simpleExpr, ("*" | "/" | "%").! map {
      case "*" => Operator.Times
      case "/" => Operator.Div
      case "%" => Operator.Mod
    })

  def simpleExpr[_: P]: P[Expression] = P(funCall | literal | name)

  def exprChain[_: P](higherPrecedence: => P[Expression],
                      op: => P[Operator]): P[Expression] =
    P(higherPrecedence ~ (op ~ higherPrecedence).rep) map {
      case (lhs, rest) =>
        rest.foldLeft(lhs) {
          case (lhs, (op, rhs)) =>
            OpExpression(op, lhs, rhs)
        }
    }
}
