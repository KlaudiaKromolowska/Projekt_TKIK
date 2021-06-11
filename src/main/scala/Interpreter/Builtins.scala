package Interpreter
import Interpreter.RuntimeValue.Callable.builtin
import Interpreter.RuntimeValue.UninitVal
import Interpreter.RuntimeValue.{
  ArrayVal,
  IntVal,
  StrVal,
  VoidVal,
  InterpreterException
}
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import parser.ast.Expression.name

/**
  * obiekt Builtins zawiera słowa kluczowe naszego programu  -
  *   takie których nie trzeba deklarować, a kompilator je zrozumie.
  *   Są to np print, readInt, a_set, a_get.
  * */
object Builtins {

  def initBuiltins(scope: myInterpreter.Scope): Option[RuntimeValue] = {
    scope.put(name("uninit"), UninitVal(name("Any")))
    scope.put(name("read"), builtin(Seq(), Some("String"), _ => {
      StrVal(StdIn.readLine())
    }))
    scope.put(name("readInt"), builtin(Seq(), Some("Int"), _ => {
      try {
        IntVal(StdIn.readInt())
      } catch {
        case _: Throwable => UninitVal(name("Any"))
      }
    }))
    scope.put(name("print"), builtin(Seq(("val", "any")), None, args => {
      print(args.head)
      VoidVal
    }))
    scope.put(name("println"), builtin(Seq(("val", "any")), None, args => {
      println(args.head)
      VoidVal
    }))
    scope.put(name("a_new"), builtin(Seq(), Some("Array"), {
      case Seq() => ArrayVal(ArrayBuffer())
      case _     => throw InterpreterException(s"a_new takes 0 argument")
    }))
    scope.put(name("a_len"), builtin(Seq(("a", "Array")), Some("Int"), {
      case Seq(ArrayVal(array)) => IntVal(array.length)
      case _                    => throw InterpreterException(s"a_len takes an array arg")
    }))
    scope.put(
      name("a_get"),
      builtin(
        Seq(("a", "Array"), ("idx", "Int")),
        Some("Any"), {
          case Seq(ArrayVal(array), IntVal(idx)) =>
            if (array isDefinedAt idx) {
              array(idx)
            } else {
              throw InterpreterException(
                s"Out of bounds read: array ${ArrayVal(array)} at index $idx"
              )
            }
          case _ =>
            throw InterpreterException(
              s"a_get takes an array and an int as args"
            )
        }
      )
    )
    scope.put(
      name("a_set"),
      builtin(
        Seq(("a", "Array"), ("idx", "Int"), ("elt", "Any")),
        None, {
          case Seq(ArrayVal(array), IntVal(idx), elt) =>
            if (array isDefinedAt idx) {
              array(idx) = elt
              VoidVal
            } else {
              throw InterpreterException(
                s"Out of bounds write: array ${ArrayVal(array)} at index $idx"
              )
            }
          case _ =>
            throw InterpreterException(
              s"a_set takes an array, an int and an object as args"
            )
        }
      )
    )
    scope.put(
      name("a_push"),
      builtin(
        Seq(("a", "Array"), ("elt", "Any")),
        None, {
          case Seq(ArrayVal(array), elt) =>
            array.append(elt)
            VoidVal
          case _ =>
            throw InterpreterException(
              s"a_push takes an array and an object as args"
            )
        }
      )
    )
    scope.put(
      name("a_rem"),
      builtin(
        Seq(("a", "Array"), ("idx", "Int")),
        None, {
          case Seq(ArrayVal(array), IntVal(idx)) =>
            if (array isDefinedAt idx) {
              array.remove(idx)
              VoidVal
            } else {
              throw InterpreterException(
                s"Out of bounds remove: array ${ArrayVal(array)} at index $idx"
              )
            }
          case _ =>
            throw InterpreterException(
              s"a_rem takes an array and an int as args"
            )
        }
      )
    )
  }
}
