package parser

class ASTTest {
    val code = "x = 5 ; y = x + 2 ;"
    val tokens = scanner.Scanner.scan(code)
   
    tokens.foreach(x => println( x.getClass().toString()))
    
    println("----AST----")
   // Parser.printTree(AST.createAST(parseTree))

}