package main

import scala.io.Source
import scanner.Scanner
import parser.Parser
import parser.Dfa
import parser.ParseException

object Joosc {

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Wrong usage")
      System.exit(1)
    }
    val file = Source.fromFile(args(0))
    val dfa = Dfa.fromFile(Source.fromFile("cfg/Joos1W.lr1"))
    try {
    	val parseTree = Parser.parse(Scanner.scan(file.mkString), dfa)
    } catch {
      case e: ParseException => 
        Console.err.println("Parse error"+e)
        System.exit(42)
    }
  }

}