package parser
object ast {
  type AST = Seq[Line]
  sealed trait Line
  sealed trait Statement extends Line
  sealed trait Expression extends Line
  case class NameSegment(name: String)

  /**
    * Statement zawiera wszystkie kluczowe funkcjonalności, takie jak pętle i pętle
    *   warunkowe,definiowanie funkcji, deklarowanie zmiennych i słowa kluczowe typu "return".
    *   Zostały tu zebrane razem dla większej przejrzystości kodu
    * */
  object Statement {
    case class Declaration(name: NameSegment, ty: Expression.Name)
        extends Statement
    case class Assignment(name: Expression.Name, expr: Expression)
        extends Statement
    case class Return(expression: Expression) extends Statement
    case class Block(lines: Seq[Line]) extends Statement
    case class IfStmt(condition: Expression,
                      ifBlock: Block,
                      elseBlock: Option[Block])
        extends Statement
    case class WhileStmt(condition: Expression, block: Block) extends Statement
    case class ForStmt(init: Option[Assignment],
                       condition: Option[Expression],
                       end: Option[Assignment],
                       block: Block)
        extends Statement
    case class FunDef(name: Expression.Name,
                      params: Seq[Declaration],
                      retTy: Option[Expression.Name],
                      code: Block)
        extends Statement
  }

  /**
    * Expression zawiera wszystkie kluczowe wyrażenia, takie jak wyrażenie z operatorami,
    *   typy zmiennych takie jak int, string czy bool.
    *   Zostały tu zebrane razem dla większej przejrzystości kodu.
    * */
  object Expression {
    def name(segments: String*): Name = Name(segments map NameSegment)

    /// Is a seq of segments for module scoping (unimplemented) and dot syntax for function calls
    case class Name(segments: Seq[NameSegment]) extends Expression {
      override def toString: String = segments.map(_.name).mkString(".")
    }
    case class FunCall(name: Name, args: Seq[Expression]) extends Expression
    case class OpExpression(operator: Operator,
                            lhs: Expression,
                            rhs: Expression)
        extends Expression
    case class IntConstant(value: Int) extends Expression
    case class StrConstant(value: String) extends Expression {
      override def toString: String = s""""$value""""
    }
    case class BoolConstant(value: Boolean) extends Expression

  }

  /**
    * Operator - zawiera tokeny wraz z opisem ich wyglądu
    * */
  sealed trait Operator {
    override def toString: String = this match {
      case Operator.Plus  => "+"
      case Operator.Minus => "-"
      case Operator.Times => "*"
      case Operator.Div   => "/"
      case Operator.GT    => ">"
      case Operator.GE    => ">="
      case Operator.LT    => "<"
      case Operator.LE    => "<="
      case Operator.Eq    => "=="
      case Operator.NEq   => "!="
      case Operator.And   => "&&"
      case Operator.Or    => "||"
      case Operator.Mod   => "%"
    }
  }

  object Operator {
    case object Plus extends Operator
    case object Minus extends Operator
    case object Times extends Operator
    case object Div extends Operator
    case object GT extends Operator
    case object GE extends Operator
    case object LT extends Operator
    case object LE extends Operator
    case object Eq extends Operator
    case object NEq extends Operator
    case object And extends Operator
    case object Or extends Operator
    case object Mod extends Operator

  }

}
