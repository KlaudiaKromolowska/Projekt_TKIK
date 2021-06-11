package Interpreter

import Interpreter.RuntimeValue._
import parser.ast.NameSegment
import parser.ast.Statement.{Block, Declaration}
import scala.collection.mutable.ArrayBuffer

trait RuntimeValue {
  override def toString: String = {
    this match {
      case VoidVal        => "void"
      case UninitVal(ty)  => s"uninit of type $ty"
      case IntVal(value)  => String.valueOf(value)
      case StrVal(value)  => value
      case BoolVal(value) => String.valueOf(value)
      case ArrayVal(values) =>
        values.map(x => x.toString).mkString(start = "[", sep = ", ", end = "]")
      case _ => super.toString
    }
  }
}

/**
  * RuntimeValue pomaga zdecydować z jakim typem zmiennych mamy doczynienia.
  * Dzięki stworzeniu wielu caseClass stworzyliśmy wiele nowych typów obiektów języka "MiniFun"
  */
object RuntimeValue {
  case object VoidVal extends RuntimeValue
  case class InterpreterException(msg: String) extends Exception {
    println("ERROR: " + msg)
  }
  case class UninitVal(ty: parser.ast.Expression.Name) extends RuntimeValue
  case class ArrayVal(values: ArrayBuffer[RuntimeValue]) extends RuntimeValue
  case class IntVal(value: Int) extends RuntimeValue
  case class StrVal(value: String) extends RuntimeValue
  case class BoolVal(value: Boolean) extends RuntimeValue
  case class TyVal(name: parser.ast.Expression.Name) extends RuntimeValue
  sealed trait Callable extends RuntimeValue

  object Callable {
    case class FunVal(params: Seq[Declaration],
                      retTy: Option[parser.ast.Expression.Name],
                      code: Block)
        extends Callable
    case class BuiltinVal(params: Seq[Declaration],
                          retTy: Option[parser.ast.Expression.Name],
                          code: Seq[RuntimeValue] => RuntimeValue)
        extends Callable

    def builtin(params: Seq[(String, String)],
                retTy: Option[String],
                code: Seq[RuntimeValue] => RuntimeValue): BuiltinVal =
      BuiltinVal(params map {
        case (ident, ty) =>
          Declaration(NameSegment(ident), parser.ast.Expression.name(ty))
      }, retTy map (parser.ast.Expression.name(_)), code)

    def params(c: Callable): Seq[NameSegment] = {
      val params = c match {
        case FunVal(params, _, _)     => params
        case BuiltinVal(params, _, _) => params
      }
      params map (_.name)
    }
  }

}
