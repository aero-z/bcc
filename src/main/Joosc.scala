package main

import scala.io.Source
import java.io.File
import scanner.Scanner
import java.io.IOException

import ast._
import main.Logger.debug
import nameResolution._
import parser._
import scanner._
import main._

case class CompilerError(str: String) extends Exception(str)

object StdlibFiles {
  val stdlibFiles = List(
    "test/javaCode/stdlib/2.0/java/lang/System.java",
    "test/javaCode/stdlib/2.0/java/lang/Object.java",
    "test/javaCode/stdlib/2.0/java/lang/Class.java",
    "test/javaCode/stdlib/2.0/java/lang/Cloneable.java",
    "test/javaCode/stdlib/2.0/java/lang/Short.java",
    "test/javaCode/stdlib/2.0/java/lang/String.java",
    "test/javaCode/stdlib/2.0/java/lang/Byte.java",
    "test/javaCode/stdlib/2.0/java/lang/Number.java",
    "test/javaCode/stdlib/2.0/java/lang/Integer.java",
    "test/javaCode/stdlib/2.0/java/lang/Character.java",
    "test/javaCode/stdlib/2.0/java/util/Arrays.java",
    "test/javaCode/stdlib/2.0/java/io/Serializable.java",
    "test/javaCode/stdlib/2.0/java/io/PrintStream.java",
    "test/javaCode/stdlib/2.0/java/io/OutputStream.java"
  ).map(f => (Source.fromFile(f), f))
  val dfa = Dfa.fromFile(Source.fromFile("cfg/grammar.lr1"))

  //def stdlibFiles = stdString.map{case (x, y) => (Source.fromString(x), y)}
  val stdAst = stdlibFiles.map{case(x, y) => AstBuilder.build(Parser.parse(Scanner.scan(x.mkString), dfa), y)} 
}


object Joosc {
  val errCodeSuccess = 0
  val errCodeParseErr = 42
  val errCodeIoErr = 1

  def check(sources: List[(Source, String)]): Int = {
    val dfa = Dfa.fromFile(Source.fromFile("cfg/grammar.lr1"))
    try {
      val compilationUnits = sources.map(source => {
        val tokens = Scanner.scan(source._1.mkString)
        debug("=== Printing tokens ===")
        tokens.foreach(debug(_))
        debug("=== Printing ast ===")
        val ast = AstBuilder.build(Parser.parse(tokens, dfa), source._2)
        //ast.display
        ast  //TODO Remove that !!!!
      }) ::: StdlibFiles.stdAst
      TypeLinking.treatAll(compilationUnits) // TODO
      errCodeSuccess
    } catch {
      case e: CompilerError =>
        Console.err.println("Compilation error: " + e.getMessage())
        errCodeParseErr
    }
  }
  
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("Wrong usage")
      System.exit(1)
    }
    val files = args.map(name =>
      try {
        (Source.fromFile(name), name)
      } catch {
        case e: IOException =>
          Console.err.println("Input error: " + e.getMessage())
          System.exit(errCodeIoErr)
          return // return is just for the compiler not to complain
      }).toList
    val ret = check(files)
    
    System.exit(ret)
  }

}
