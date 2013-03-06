package nameResolution

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._
import ast._

class VarLinking extends FunSuite {
  val classes = List(
    ("B.java","""
package p;

public class B  {
public int l;
protected String l;
public void t(){
{
int l;
}
}
}
"""))
  
  def createCU(string:String, name:String):CompilationUnit = {
    //println("BEGIN TEST")
    val tokens = Scanner.scan(string)
    //tokens.foreach(x => println(x.getClass()))
    //println("SCANNING DONE")
    val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/grammar.lr1")))
    //println("BUILDING DFA DONE")
    val parseTree:ParserSymbol = Parser.parse(tokens, dfa)
    //println("BUILDING PARSETREE DONE")
    //Parser.printTree(parseTree)
    //println("BUILDING AST TREE")
    val ast = AstBuilder.build(parseTree: ParserSymbol, name)
    println("DONE CREATING THE Compilation Unit!")
    ast
  }

  test("TypeLinking") {
    val list = classes.map(x => createCU(x._2, x._1))
    
    
    val linked = VarResolver.variableLink(list)
    
    //goToParent(linked(1).typeDef.get, 0)
    println(linked(0))
    
    


  }
}

