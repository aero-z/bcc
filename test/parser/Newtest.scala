package parser

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._

class Newtest extends FunSuite {
    val codeList = List("""class myClass {}""",
            """package edu.syr.pcpratts.jpp.templates;
import edu.syr.pcpratts.jpp.cfile.CFileItem;
import edu.syr.pcpratts.jpp.cfile.CFileVisitor;
class MethodDeclFinderPass extends CFileVisitor {
  public MethodDeclFinderPass() {
    int rouge = 32;
            rouge.salope = rouge;
            }
  protected void visitItem(CFileItem item) {
  }
}""")
    val myCode = """class myClass {
}"""
    test("Everything") {
        codeList foreach testCode
    }

    def testCode(code: String) {
        val tokens = Scanner.scan(code)
        tokens.foreach(x => println(s"${x.getClass()} ${x.typeStr}"))
        println("SCANNING DONE")
        val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/eclipse.lr1")))
        println("BUILDING DFA DONE")
        val parseTree = Parser.parse(tokens, dfa)
        println("BUILDING PARSETREE DONE")
        Parser.printTree(parseTree)
        println("==========================")
        println("TEST DONE")
        println("==========================")
    }
}