package nameResolution

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._
import ast._

class Linking extends FunSuite {
  val classes = List(
    ("whateve/Je_2_Locals_Overlapping_DeeplyNested.java","""
public class Je_2_Locals_Overlapping_DeeplyNested {
	public Je_2_Locals_Overlapping_DeeplyNested() {}
	
	public static int test() {
		int a = 123;
		boolean b = true;
		boolean c = true;
		boolean d = true;
		boolean e = true;
		boolean f = true;
  		if (b) if (c) if (d) if (e) if (f) { int a = 43; return a+80; }
		return a;
	}
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
      linked.foreach(printAddress(_))
      

    } catch {case e:Exception => println("The exception: "+e.printStackTrace()+" "+e.getMessage())}
  }
}
