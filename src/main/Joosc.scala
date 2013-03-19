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
import typecheck.TypeChecker
import reachability._


abstract class CompilerError(mess: String, typeErr: String) extends Exception(mess){
  val errorMessage = s"[$typeErr]: $mess"
}

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
  val addStdLib = true
  
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
        ast.display
        ast
      }) ::: (if (addStdLib) StdlibFiles.stdAst else Nil)
      val typeLinked = TypeLinking.treatAll(compilationUnits)
      
      HierarchyChecking.checkHierarchy(typeLinked)
      val varLinked = VarResolver.variableLink(typeLinked)
      varLinked.foreach(_.display)
      TypeChecker.check(varLinked)
      //varLinked.flatMap{case CompilationUnit(_, _, Some(c:ClassDefinition), _) => c.methods case _ => Nil }.foreach(FinitePath.check(_))
      
      errCodeSuccess
    } catch {
      case e: CompilerError =>
        Console.err.println(e.errorMessage)//+"\n"+e.printStackTrace())
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
