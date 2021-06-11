package Interpreter

import Interpreter.RuntimeValue.Callable.{BuiltinVal, FunVal}
import Interpreter.RuntimeValue._
import parser.ast.Expression._
import parser.ast.Operator._
import parser.ast.Statement._
import parser.ast.{AST, Expression, Operator, Statement}
import scala.collection.mutable

object myInterpreter {
  type Scope = mutable.HashMap[parser.ast.Expression.Name, RuntimeValue]
  val globalValues: Scope = mutable.HashMap()
  Builtins.initBuiltins(globalValues)
  def eval(ast: AST): Unit = evalScope(ast, List())

  /**
    * Zwracamy wynik, jezeli w programie jezyka "MiniFun" byla dostepne "return".
    * Zmienne i wyrazenia dostepne w programie są używane jako stos (najpierw szukamy nazw lokalnie, pozniej globalnie)
    * Pierwszy element to bieżący zakres parametrów
    *
    * @param ast - abstract syntax tree - drzewo składniowe - to podstawa składni używanej do reprezentowania programów
    * @param outerScope - zewnętrzny blok kodu
    * */
  def evalScope(ast: AST, outerScope: List[Scope]): Option[RuntimeValue] = {
    val reachable = mutable
      .HashMap[parser.ast.Expression.Name, RuntimeValue]() :: outerScope
    for (line <- ast) {
      line match {
        case stmt: Statement =>
          evalStatement(stmt, reachable) match {
            case Some(retVal) => return Some(retVal)
            case None         => ()
          }
        case expr: Expression => evalExpression(expr, reachable)
      }
    }
    None
  }

  /** Ta funkcja odpowiada za przypasowanie wyrażenia będącego częścią programu
    *
    * @param statement - w zależności od tego czym jest statement, wywoływana jest odpowiednia funkcja,
    *                  np. gdy statement to IfStmt, to wywkonywana jest odpowiednia instrukcja warunkowa.
    *
    * @param reachable - lista wyrażeń wchodzących w skład bloku kodu
    * */
  def evalStatement(statement: Statement,
                    reachable: List[Scope]): Option[RuntimeValue] = {
    statement match {
      case Declaration(nameSegment, ty) =>
        reachable.head(parser.ast.Expression.Name(Seq(nameSegment))) =
          UninitVal(ty)
        None
      case Assignment(name, expr) => evalAssignment(name, expr, reachable)
      case Block(lines)           => evalScope(lines, reachable)
      case IfStmt(condition, ifBlock, elseBlock) =>
        if (evalCondition(condition, reachable)) {
          evalScope(ifBlock.lines, reachable)
        } else {
          elseBlock.flatMap(block => evalScope(block.lines, reachable))
        }
      case WhileStmt(condition, block) =>
        while (evalCondition(condition, reachable)) {
          evalScope(block.lines, reachable) match {
            case Some(retVal) => return Some(retVal)
            case None         => ()
          }
        }
        None
      case ForStmt(init, condition, end, block) =>
        init.foreach(evalStatement(_, reachable))
        evalStatement(
          WhileStmt(
            condition.getOrElse(BoolConstant(true)),
            Block(block.lines ++ end.map(Seq(_)).getOrElse(Seq()))
          ),
          reachable
        )
      case FunDef(name, params, retTy, code) =>
        reachable.head(name) = FunVal(params, retTy, code)
        None
      case Return(expression) => Some(evalExpression(expression, reachable))
    }
  }

  /** Funkcja zwraca nie tylko blok kodu, ale również buforowaną wartość, co pozwala na łatwiejsze wykrywanie błędów kompilacji.
    * Np. w przypadku gdy zmienna nie została prawidłowo przypisana lub nie istnieje podane wyrażenie
    */
  def valueLookup(name: parser.ast.Expression.Name,
                  reachable: List[Scope]): Option[(Scope, RuntimeValue)] = {
    for (scope <- reachable) {
      scope.get(name) match {
        case Some(value) => return Some((scope, value))
        case None        => ()
      }
    }
    globalValues.get(name).map(value => (globalValues, value))
  }

  /**
    * W funkcjach "evalAssigment", "evalCondition", "evalExpression", "evalFunCall", "evalOpExpression"
    * wykonywane są odpowiednio przypisanie, sprawdzenie warunków, wykonanie wyrażenia, funkcji oraz wyrażenia składającego się z operatorów.
    * Metodyka działania funkcji jest podobna, wszystkie pobierają odpowiednie wyrażenia i rozkłądają je na mniejsze części,
    * umożliwiając kompilację programu w języku MiniFun
    * */
  def evalAssignment(name: parser.ast.Expression.Name,
                     expr: Expression,
                     reachable: List[Scope]): Option[RuntimeValue] = {
    valueLookup(name, reachable) match {
      case Some((scope, _)) =>
        val newVal = evalExpression(expr, reachable)
        scope(name) = newVal
      case None =>
        throw InterpreterException(
          s"Tried to assign to undeclared variable $name"
        )
    }
    None
  }

  def evalCondition(condition: Expression, reachable: List[Scope]): Boolean = {
    evalExpression(condition, reachable) match {
      case BoolVal(true)  => true
      case BoolVal(false) => false
      case other =>
        throw InterpreterException(
          s"Value $condition = $other is not a boolean"
        )
    }
  }

  def evalExpression(expression: Expression,
                     reachable: List[Scope]): RuntimeValue = {
    expression match {
      case name: parser.ast.Expression.Name =>
        valueLookup(name, reachable) match {
          case None =>
            throw InterpreterException(
              s"Name $name is not reachable (reachable scopes: $reachable"
            )
          case Some((_, value)) => value
        }
      case FunCall(name, args) => evalFunCall(name, args, reachable)
      case OpExpression(operator, lhs, rhs) =>
        evalOpExpression(operator, lhs, rhs, reachable)
      case IntConstant(int)   => IntVal(int)
      case StrConstant(str)   => StrVal(str)
      case BoolConstant(bool) => BoolVal(bool)
    }
  }

  def evalFunCall(name: parser.ast.Expression.Name,
                  args: Seq[Expression],
                  reachable: List[Scope]): RuntimeValue = {
    val function = valueLookup(name, reachable) match {
      case Some((_, value)) =>
        value match {
          case c: Callable => c
          case _ =>
            throw InterpreterException(
              s"$name is not a function but you tried calling it"
            )
        }
      case None =>
        throw InterpreterException(
          s"Function $name is not reachable (reachable scopes: $reachable"
        )
    }
    import RuntimeValue.Callable.params
    if (args.length != params(function).length) {
      throw InterpreterException(
        s"Function $name must be called with ${params(function).length} arguments"
      )
    }

    val argValues = args map (evalExpression(_, reachable))

    function match {
      case FunVal(_, _, code) =>
        // Arguments are in a separate scope because it's easier
        val argScope =
          mutable.HashMap[parser.ast.Expression.Name, RuntimeValue]()
        for ((name, arg) <- params(function) zip argValues) {
          argScope.put(Expression.Name(Seq(name)), arg)
        }
        evalScope(code.lines, List(argScope)) match {
          case Some(retVal) => retVal
          case None         => VoidVal
        }
      case BuiltinVal(_, _, code) => code(argValues)
    }
  }

  def evalOpExpression(operator: Operator,
                       lhs: Expression,
                       rhs: Expression,
                       reachable: List[Scope]): RuntimeValue = {
    val leftValue = evalExpression(lhs, reachable)
    val rightValue = evalExpression(rhs, reachable)
    (operator, leftValue, rightValue) match {
      case (Plus, IntVal(l), IntVal(r))      => IntVal(l + r)
      case (Minus, IntVal(l), IntVal(r))     => IntVal(l - r)
      case (Times, IntVal(l), IntVal(r))     => IntVal(l * r)
      case (Div, IntVal(l), IntVal(r))       => IntVal(l / r)
      case (Mod, IntVal(l), IntVal(r))       => IntVal(l % r)
      case (GT, IntVal(l), IntVal(r))        => BoolVal(l > r)
      case (GE, IntVal(l), IntVal(r))        => BoolVal(l >= r)
      case (LT, IntVal(l), IntVal(r))        => BoolVal(l < r)
      case (LE, IntVal(l), IntVal(r))        => BoolVal(l <= r)
      case (Eq, UninitVal(_), UninitVal(_))  => BoolVal(true)
      case (NEq, UninitVal(_), UninitVal(_)) => BoolVal(false)
      case (Eq, l, r)                        => BoolVal(l == r)
      case (NEq, l, r)                       => BoolVal(l != r)
      case (And, BoolVal(l), BoolVal(r))     => BoolVal(l && r)
      case (Or, BoolVal(l), BoolVal(r))      => BoolVal(l || r)
      case _ =>
        throw InterpreterException(
          s"Unhandled operation $leftValue $operator $rightValue"
        )
    }
  }

}
