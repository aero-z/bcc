package main

import scala.io.Source
import java.io.File
import scanner.Scanner
import parser.Parser
import parser.Dfa
import parser.Weeder
import java.io.IOException
import main.Logger.debug
import parser.ParserSymbol
import parser.NonTerminalSymbol
import parser.NonTerminalSymbol
import parser.NonTerminalSymbol
import scanner.IdentifierToken
import ast.AstBuilder
import ast.CheckResult
import ast.CheckOk
import ast.CheckFail

case class CompilerError(str: String) extends Exception(str)

object Joosc {

    val errCodeSuccess = 0
    val errCodeParseErr = 42
    val errCodeIoErr = 1
    
    def check(source: Source, name: String): Int = {
        val dfa = Dfa.fromFile(Source.fromFile("cfg/grammar.lr1"))
        try {
            val tokens = Scanner.scan(source.mkString)
            debug("=== Printing tokens ===")
            tokens.foreach(debug(_))
            debug("=== Printing ast ===")
            val ast = AstBuilder.build(Parser.parse(tokens, dfa), name)
            ast.display
            Weeder.check(ast) match {
              case CheckOk() => errCodeSuccess
              case CheckFail(err) => Console.err.println(err); errCodeParseErr
            }
        } catch {
            case e: CompilerError =>
                Console.err.println("Compilation error: " + e.getMessage())
                errCodeParseErr
        }

    }

    def main(args: Array[String]): Unit = {
        if (args.length != 1) {
            println("Wrong usage")
            System.exit(1)
        }
        val file: Source =
            try {
                Source.fromFile(args(0))
            } catch {
                case e: IOException =>
                    Console.err.println("Input error: " + e.getMessage())
                    System.exit(errCodeIoErr)
                    return // return is just for the compiler not to complain
            }
        val ret = check(file, args(0))
        System.exit(ret)
    }

}
