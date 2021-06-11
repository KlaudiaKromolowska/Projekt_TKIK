import org.scalatest.funsuite._

import parser.ast.Expression.{name, _}
import parser.ast.NameSegment
import parser.ast.Operator._
import parser.ast.Statement._
import fastparse._
import parser.parser.program

class ParserTest extends AnyFunSuite {

  val singleVar = "test;"
  test(s"single variable in a line : '$singleVar'") {
    val actual = parse(singleVar, program(_))
    val expected = Parsed.Success(
      Seq(Name(Seq(NameSegment(singleVar.substring(0, singleVar.length - 1))))),
      5
    )
    assert(actual === expected)
  }

  val multiVar = "test_var; test;"
  test(s"Two variables : '$multiVar'") {
    val actual = parse(multiVar, program(_))
    val expected = Parsed.Success(
      Seq(Name(Seq(NameSegment("test_var"))), Name(Seq(NameSegment("test")))),
      15
    )
    assert(actual === expected)
  }

  val multiVarTwoLines = "test_var;\n test;"
  test(s"Two variables in two lines: '$multiVarTwoLines'") {
    val actual = parse(multiVarTwoLines, program(_))
    val expected = Parsed.Success(
      Seq(Name(Seq(NameSegment("test_var"))), Name(Seq(NameSegment("test")))),
      16
    )
    assert(actual === expected)
  }

  val dottedName = "a.bc.de.f;"
  test(s"Notation with dots: '$dottedName'") {
    val actual = parse(dottedName, program(_))
    val expected = Parsed.Success(
      Seq(
        Name(
          Seq(
            NameSegment("a"),
            NameSegment("bc"),
            NameSegment("de"),
            NameSegment("f")
          )
        )
      ),
      10
    )
    assert(actual === expected)
  }

  val simpleIfStmt: String =
    """if machine {
      |  runMachine;
      |}""".stripMargin
  test(s"if simple : '$simpleIfStmt'") {
    val actual = parse(simpleIfStmt, program(_))
    val expected = Parsed.Success(
      Seq(
        IfStmt(
          Name(Seq(NameSegment("machine"))),
          Block(Seq(Name(Seq(NameSegment("runMachine"))))),
          None
        )
      ),
      28
    )
    assert(actual === expected)
  }

  val ifStmtWithElse: String =
    """if machine {
      |  runMachine;
      |} else {
      |  stopWork;
      |}
      |""".stripMargin
  test(s"if with else statement: '$ifStmtWithElse'") {
    val actual = parse(ifStmtWithElse, program(_))
    val expected = Parsed.Success(
      Seq(
        IfStmt(
          Name(Seq(NameSegment("machine"))),
          Block(Seq(Name(Seq(NameSegment("runMachine"))))),
          Some(Block(Seq(Name(Seq(NameSegment("stopWork"))))))
        )
      ),
      50
    )
    assert(actual === expected)
  }

  val simpleFunCalls = "testFn(randomArg); testFn(); testFn(arg1, arg2);"
  test(s"function calls: '$simpleFunCalls'") {
    val actual = parse(simpleFunCalls, program(_))
    val expected = Parsed.Success(
      Seq(
        FunCall(
          Name(Seq(NameSegment("testFn"))),
          Seq(Name(Seq(NameSegment("randomArg"))))
        ),
        FunCall(Name(Seq(NameSegment("testFn"))), Seq()),
        FunCall(
          Name(Seq(NameSegment("testFn"))),
          Seq(Name(Seq(NameSegment("arg1"))), Name(Seq(NameSegment("arg2"))))
        )
      ),
      48
    )
    assert(actual === expected)
  }

  val nestedFunCalls: String = "foo(bar(baz));"
  test(s"nested function calls : '$nestedFunCalls'") {
    val actual = parse(nestedFunCalls, program(_))
    val expected = Parsed.Success(
      Seq(
        FunCall(
          name("foo"),
          Seq(
            FunCall(
              Name(Seq(NameSegment("bar"))),
              Seq(Name(Seq(NameSegment("baz"))))
            )
          )
        )
      ),
      14
    )
    assert(actual === expected)
  }

  val priorities = "a + b * c + d / e * f;"
  test(s"priorities : '$priorities'") {
    val actual = parse(priorities, program(_))
    val expected = Parsed.Success(
      Seq(
        OpExpression(
          Plus,
          OpExpression(
            Plus,
            name("a"),
            OpExpression(Times, name("b"), name("c"))
          ),
          OpExpression(
            Times,
            OpExpression(Div, name("d"), name("e")),
            name("f")
          )
        )
      ),
      22
    )
    assert(actual === expected)
  }

  val assignment = "a = foo(bar);"
  test(s"assignment : '$assignment'") {
    val actual = parse(assignment, program(_))
    val expected = Parsed.Success(
      Seq(
        Assignment(
          Name(Seq(NameSegment("a"))),
          FunCall(
            Name(Seq(NameSegment("foo"))),
            Seq(Name(Seq(NameSegment("bar"))))
          )
        )
      ),
      13
    )
    assert(actual === expected)
  }

  val declaration = "foo: int;"
  test(s"declaration : '$declaration'") {
    val actual = parse(declaration, program(_))
    val expected = Parsed.Success(
      Seq(Declaration(NameSegment("foo"), Name(Seq(NameSegment("int"))))),
      9
    )
    assert(actual === expected)
  }

  val twoPlusTwo = "2 + 2;"
  test(s"addition simple : $twoPlusTwo") {
    val actual = parse(twoPlusTwo, program(_))
    val expected =
      Parsed.Success(Seq(OpExpression(Plus, IntConstant(2), IntConstant(2))), 6)
    assert(actual === expected)
  }

  val doubleAddition = "1 + 2 + 42;"
  test(s"compicated addition: $doubleAddition") {
    val actual = parse(doubleAddition, program(_))
    val expected = Parsed.Success(
      Seq(
        OpExpression(
          Plus,
          OpExpression(Plus, IntConstant(1), IntConstant(2)),
          IntConstant(42)
        )
      ),
      11
    )

    assert(actual === expected)
  }
}
