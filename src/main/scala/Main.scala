import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Paths}

import Interpreter.myInterpreter.eval
import parser.parser.parseProgram

import scala.collection.JavaConverters._

object Main extends App {

  /**
    * main - główna funckja całego programu.
    * readFromFile - funkcja która odczytuje nam zawartość pliku
    * addPath - funkcja pyta użytkownika o nazwę pliku do skompilowania
    * compile - odpowiada za kompilację pliku znajdującego się pod określoną ścieżką
    * wrongPath - wykonywane w przypadku podania złej nazwy pliku,
    *   po ponownym podaniu nazwy pliku ponownie wywołuje metode "compile"
    * */
  override def main(args: Array[String]): Unit = {
    addPath()
  }

  def readFromFile(name: String): String = {
    Files.readAllLines(Paths.get(name)).asScala.mkString("\n")
  }

  def addPath(): Unit = {
    val currentDirectory = new java.io.File(".").getCanonicalPath
    println("Please, write name of your file below. Example: myCode1.txt")
    val myFile = readLine()
    val path = currentDirectory + "\\code-samples\\" + myFile
    compile(path)
  }

  def compile(path: String): Unit = {
    val actual = new ByteArrayOutputStream
    if (Files.exists(Paths.get(path))) {
      val myCode = readFromFile(path)

      val program = parseProgram(myCode.stripMargin).get.value
      //println(program)
      println(
        "Compiling... \n" +
          "Your code: \n" + myCode +
          "\n \nAnswer: "
      )
      Console.withOut(actual) { eval(program) }
      if (actual.toString != "") {
        eval(program)
      } else {
        println("SYNTAX ERROR")
      }
    } else wrongPath()
  }

  def wrongPath(): Unit = {
    val currentDirectory = new java.io.File(".").getCanonicalPath
    println(
      "There is no file like that. Try once again. \n" +
        "You can use example code: myCode1.txt"
    )
    val myFile = readLine()
    val path = currentDirectory + "\\code-samples\\" + myFile
    compile(path)
  }
}
