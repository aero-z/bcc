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

case class CompilerError(str: String) extends Exception(str)

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
        ast
      })
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
