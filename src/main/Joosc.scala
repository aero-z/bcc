package main

import scala.io.Source
import java.io.File
import parser.Ast
import scanner.Scanner
import parser.Parser
import parser.Dfa
import parser.Weeder
import java.io.IOException
import main.Logger.debug
import parser.Symbol
import parser.NonTerminalSymbol
import parser.NonTerminalSymbol
import parser.NonTerminalSymbol
import scanner.IdentifierToken
import abstractSyntaxTree.ASTBuilder

class CompilerError(str: String) extends Exception(str)

object Joosc {

    val errCodeSuccess = 0
    val errCodeParseErr = 42
    val errCodeIoErr = 1

    def check(source: Source, name: String): Int = {
        val dfa = Dfa.fromFile(Source.fromFile("cfg/grammar.lr1"))
        val parseTree =
            try {
                val tokens = Scanner.scan(source.mkString)
                debug("=== Printing tokens ===")
                tokens.foreach(debug(_))
                debug("=== Printing ast ===")
                ASTBuilder.build(Parser.parse(tokens, dfa), name).display
            } catch {
                case e: CompilerError =>
                    Console.err.println("Syntax error while parsing: " + e.getMessage())
                    return errCodeParseErr
            }

        errCodeSuccess
    }

    def checkFileName(tree: Symbol, name: String): Boolean = {
        val fileName = new File(name).getName
        if (!fileName.endsWith(".java")) false
        val className = fileName.substring(0, fileName.length - 5)
        tree match {
            case NonTerminalSymbol("CompilationUnit", list) =>
                list.exists(_ match {
                    case NonTerminalSymbol("ClassDeclaration", xs) => xs contains IdentifierToken(className)
                    case NonTerminalSymbol("InterfaceDeclaration", xs) => xs contains IdentifierToken(className)
                    case _ => false
                }
            )
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
