package nameResolution

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._
import ast._

class Linking extends FunSuite {
  val classes = List(
    ("C1.java","""
package pk1;

import pk2.C2;

public class C1 extends C2 {
	public C1() {}
	public void foo() {
		C1 f = new C1();
		C2 a;
	}
}
"""),
    ("C2.java", """
package pk2;

import pk1.C1;
    
public class C2 extends C1 {
	public C2() {}
}
""")
  )
  def printAddress(cu:CompilationUnit) =
    println("XXXXX "+cu.typeName+": "+cu.typeDef.get.hashCode)
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
    list.foreach(printAddress(_))
    try {
      val linked = TypeLinking.treatAll(list)
      /*def goToParent(cu: TypeDefinition, depth: Int): Unit = cu match{
        case ClassDefinition(name, Some(RefTypeLinked(papa, cl)),_ ,_ ,_,_, _)=> println(s"$papa is papa of $name with $depth"); goToParent(cl, depth+1)
        case _ => println("Shit")
      }*/
      //goToParent(linked(1).typeDef.get, 0)
      println(linked(1))
      linked.foreach(printAddress(_))
      

    } catch {case e:Exception => println("The exception: "+e.toString()+" "+e.getMessage())}
  }
}
