package main

import scala.io.Source
import java.io.File
import scanner.Scanner
import parser.Parser
import parser.Dfa
import parser.Weeder
import java.io.IOException
import main.Logger.debug

class CompilerError(str: String) extends Exception(str)

object Joosc {

    val errCodeSuccess = 0
    val errCodeParseErr = 42
    val errCodeIoErr = 1

    def check(source: Source, name: String): Int = {
        val fileName = new File(name).getName
        if (!fileName.endsWith(".java")) return errCodeParseErr
        val className = fileName.substring(0, fileName.length - 6)
        if (!Character.isJavaIdentifierStart(className.head) || className.tail.exists(!Character.isJavaIdentifierPart(_))) return errCodeParseErr
        val dfa = Dfa.fromFile(Source.fromFile("cfg/grammar.lr1"))
        val parseTree =
            try {
                val tokens = Scanner.scan(source.mkString)
                debug("=== Printing tokens ===")
                tokens.foreach(debug(_))
                val parseTree = Parser.parse(tokens, dfa)
                debug("=== Printing parse tree ===")
                Parser.printTree(parseTree)
                parseTree
            } catch {
                case e: CompilerError =>
                    Console.err.println("Syntax error while parsing: " + e.getMessage())
                    return errCodeParseErr
            }

        errCodeSuccess
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
