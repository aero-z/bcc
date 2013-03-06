package main

import scala.io.Source
import java.io.File
import scanner.Scanner
import parser.Parser
import parser.Dfa
import java.io.IOException
import main.Logger.debug
import parser.ParserSymbol
import parser.NonTerminalSymbol
import parser.NonTerminalSymbol
import parser.NonTerminalSymbol
import scanner.IdentifierToken
import ast._
import nameResolution.TypeLinking

case class CompilerError(str: String) extends Exception(str)

object Joosc {
  val errCodeSuccess = 0
  val errCodeParseErr = 42
  val errCodeIoErr = 1

  def check(sources: List[Source]): Int = {
    val dfa = Dfa.fromFile(Source.fromFile("cfg/grammar.lr1"))
    try {
      val compilationUnits = sources.map(source => {
        val tokens = Scanner.scan(source.mkString)
        debug("=== Printing tokens ===")
        tokens.foreach(debug(_))
        debug("=== Printing ast ===")
        val ast = AstBuilder.build(Parser.parse(tokens, dfa), source.descr)
        //ast.display
        ast
      })
      //TypeLinking.treatAll(compilationUnits) // TODO
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
    val files = args.map(
      try {
        Source.fromFile(_)
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
