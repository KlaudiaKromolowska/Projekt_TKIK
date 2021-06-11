import java.io.ByteArrayOutputStream

import Interpreter.myInterpreter.eval
import org.scalatest.funsuite._
import parser.parser.parseProgram

class InterpreterTest extends AnyFunSuite {

  test("simple println") {
    val actual = new ByteArrayOutputStream
    val expected = "println"
    val program = parseProgram(s"""println("$expected");""").get.value
    Console.withOut(actual) {
      eval(program)
    }
    val myActual = actual.toString.replaceAll("\r\n+$", "")
    assert(myActual === expected.replaceAll("\n", ""))
  }

  test("simple if") {
    val actual = new ByteArrayOutputStream
    val expected = "if"
    val program = parseProgram(s"""machin: Boolean;
         |machin = true;
         |if machin {
         |  print("$expected");
         |}""".stripMargin).get.value
    Console.withOut(actual) {
      eval(program)
    }
    assert(actual.toString === expected)
  }

  test("else") {
    val actual = new ByteArrayOutputStream
    val expected = "else"
    val program = parseProgram(s"""machin: Boolean;
         |machin = false;
         |if machin {
         |  print("oops");
         |} else {
         |  print("$expected");
         |}""".stripMargin).get.value
    Console.withOut(actual) {
      eval(program)
    }
    assert(actual.toString === expected)
  }

  test("while") {
    val actual = new ByteArrayOutputStream
    val expected = "whale\nwhale\nwhale\n"
    val program = parseProgram(s"""machin: Int;
         |machin = 3;
         |while machin > 0 {
         |  println("whale");
         |  machin = machin - 1;
         |}""".stripMargin).get.value
    Console.withOut(actual) {
      eval(program)
    }

    assert(
      actual.toString.replaceAll("\\s", "") === expected.replaceAll("\\s", "")
    )
  }

  test("for") {
    val actual = new ByteArrayOutputStream
    val expected = "abc\nabc\nabc"
    val program = parseProgram(s"""machin: Int;
         |for machin = 3; machin > 0; machin = machin - 1 {
         |  println("abc");}""".stripMargin).get.value

    Console.withOut(actual) {
      eval(program)
    }
    assert(
      actual.toString.replaceAll("\\s", "") === expected.replaceAll("\\s", "")
    )
  }

  test("arrays") {
    val actual = new ByteArrayOutputStream
    val expected = "4\n2\n"
    val program = parseProgram(s"""array: Array;
         |array = a_new();
         |i: Int;
         |for i = 0; i < 3; i = i + 1 {
         |  a_push(array, i);
         |}
         |a_rem(array, 2);
         |
         |a_set(array, 0, 4);
         |a_set(array, 1, 2);
         |
         |for i = 0; i < a_len(array); i = i + 1 {
         | println(a_get(array, i));
         |}
         |""".stripMargin).get.value
    println(program)
    Console.withOut(actual) {
      eval(program)
    }

    assert(
      actual.toString.replaceAll("\\s", "") === expected.replaceAll("\\s", "")
    )
  }

  test("multiple assignments") {
    val actual = new ByteArrayOutputStream
    val expected = "0"
    val program = parseProgram(s"""machin: Int;
         |machin = 3;
         |machin = machin - 1;
         |machin = machin - 1;
         |machin = machin - 1;
         |print(machin);
         |""".stripMargin).get.value

    Console.withOut(actual) {
      eval(program)
    }

    assert(actual.toString === expected)
  }

  test("procedure") {
    val actual = new ByteArrayOutputStream
    val expected = "1\n"
    val program = parseProgram(s"""fn myprint(arg: Any) {
         |  println(arg);
         |}
         |myprint(1);
         |""".stripMargin).get.value
    Console.withOut(actual) {
      eval(program)
    }

    assert(
      actual.toString.replaceAll("\\s", "") === expected.replaceAll("\\s", "")
    )
  }

  test("function") {
    val actual = new ByteArrayOutputStream
    val expected = "1\n"
    val program = parseProgram(s"""fn one(): Int {
         |  return 1;
         |}
         |println(one());
         |""".stripMargin).get.value
    Console.withOut(actual) {
      eval(program)
    }

    assert(
      actual.toString.replaceAll("\\s", "") === expected.replaceAll("\\s", "")
    )
  }

  test("better function") {
    val actual = new ByteArrayOutputStream
    val expected = "42\n"
    val program = parseProgram(s"""fn myadd(x: Int, y: Int): Int {
         |  return x + y;
         |}
         |println(myadd(1, 41));
         |""".stripMargin).get.value
    Console.withOut(actual) {
      eval(program)
    }

    assert(
      actual.toString.replaceAll("\\s", "") === expected.replaceAll("\\s", "")
    )
  }

  test("abs function") {
    val actual = new ByteArrayOutputStream
    val expected = "42\n"
    val program = parseProgram(s"""fn abs(x: Int): Int {
         |  if x > 0 {
         |    return x;
         |  } else {
         |    return 0 - x;
         |  }
         |}
         |println(abs(0 - 42));
         |""".stripMargin).get.value
    Console.withOut(actual) {
      eval(program)
    }

    assert(
      actual.toString.replaceAll("\\s", "") === expected.replaceAll("\\s", "")
    )
  }
}
