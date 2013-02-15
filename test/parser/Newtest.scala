package parser

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._

class Newtest extends FunSuite{
  val myCode = """
public class J1_1_AmbiguousName_AccessResultFromMethod{

    public int i;

    public J1_1_AmbiguousName_AccessResultFromMethod(int j){
    i = j;
    }

    public J1_1_AmbiguousName_AccessResultFromMethod inc(){
    return new J1_1_AmbiguousName_AccessResultFromMethod(i+1);
    }

    public static int test(){
    return new J1_1_AmbiguousName_AccessResultFromMethod(120).inc().inc().inc().i;
    }

}
"""
  test("Everything") {
	println("BEGIN TEST")
    val tokens = Scanner.scan(myCode)
	tokens.foreach(x => println(x.getClass()))
	println("SCANNING DONE")
	val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/grammar.lr1")))
	println("BUILDING DFA DONE")
	val parseTree = Parser.parse(tokens, dfa)
	println("BUILDING PARSETREE DONE")
	Parser.printTree(parseTree)
  }
}
