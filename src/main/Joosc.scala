package main

import scala.io.Source
import scanner.Scanner
import parser.Parser
import parser.Dfa
import parser.ParseException

object Joosc {
  
  val errCodeSuccess = 0
  val errCodeParseErr = 42
  
  def compile(source: Source): Int = {
    val dfa = Dfa.fromFile(Source.fromFile("cfg/Joos1W.lr1"))
    try {
    	val parseTree = Parser.parse(Scanner.scan(source.mkString), dfa)
    } catch {
      case e: ParseException => 
        Console.err.println("Parse error")
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
    val ret = compile(file)
    System.exit(ret)
  }

}