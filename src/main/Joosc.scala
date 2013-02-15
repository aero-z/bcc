package main

import scala.io.Source
import scanner.Scanner
import parser.Parser
import parser.Dfa
import parser.ParseException
import parser.Weeder

object Joosc {
  
  val errCodeSuccess = 0
  val errCodeParseErr = 42
  
  def check(source: Source): Int = {
    val dfa = Dfa.fromFile(Source.fromFile("cfg/grammar.lr1"))
    val parseTree =
    try {
    	Parser.parse(Scanner.scan(source.mkString), dfa)
    } catch {
      case e: ParseException => 
        Console.err.println("Syntax error while parsing")
        return errCodeParseErr
    }
    if (Weeder.check(parseTree) == false) {
        Console.err.println("Syntax error while weeding")
        return errCodeParseErr
    }
      
    errCodeSuccess
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Wrong usage")
      System.exit(1)
    }
    val file = Source.fromFile(args(0))
    val ret = check(file)
    System.exit(ret)
  }

}
