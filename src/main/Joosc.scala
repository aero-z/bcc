package main

import scala.io.Source
import java.io.File
import scanner.Scanner
import java.io.IOException
import ast._
import ast.Type
import main.Logger.debug
import nameResolution._
import parser._
import scanner._
import typecheck.TypeChecker
import reachability._


abstract class CompilerError(msg: String, typeErr: String) extends Exception(msg){
  val errorMessage = s"[$typeErr]: $msg"
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

  val stdAst = stdlibFiles.map{case(x, y) => AstBuilder.build(Parser.parse(Scanner.scan(x.mkString), dfa), y)}
}


object Joosc {
  val addStdLib = true    // whether the std lib file will be added automatically to the build 
  val linkJavaLang = true // whether java.lang should be part of the language and Object should be base of every class
  
  val errCodeSuccess = 0
  val errCodeCompileErr = 42
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
      //
      varLinked
      //.filter(_.packageName != Some(Name("java"::"lang"::Nil))).filter(_.packageName != Some(Name("java"::"io"::Nil))).filter(_.packageName != Some(Name("java"::"util"::Nil)))
      .flatMap{case CompilationUnit(_, _, Some(c:ClassDefinition), name) => debug(">>>>>>>>>> "+name); c.methods case _ => Nil }.filter(x => !x.modifiers.contains(Modifier.abstractModifier) && !x.modifiers.contains(Modifier.nativeModifier)).foreach(FinitePath.check(_))
      
      varLinked //for the constructors, kind of an ugly hack
      //.filter(_.packageName != Some(Name("java"::"lang"::Nil))).filter(_.packageName != Some(Name("java"::"io"::Nil))).filter(_.packageName != Some(Name("java"::"util"::Nil)))
      .flatMap{case CompilationUnit(_, _, Some(c:ClassDefinition), name) => debug(">>>>>>>>>> "+name); c.constructors case _ => Nil }.filter(x => !x.modifiers.contains(Modifier.abstractModifier) && !x.modifiers.contains(Modifier.nativeModifier)).foreach(x => FinitePath.check(MethodDeclaration(x.name, VoidType, x.modifiers, x.parameters, Some(x.implementation))))
      
      errCodeSuccess
    } catch {
      case e: CompilerError =>
        Console.err.println(e.errorMessage)//+"\n"+e.printStackTrace())
        errCodeCompileErr
    }
  }
  
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      debug("Wrong usage")
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
