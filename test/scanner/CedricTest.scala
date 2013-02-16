package scanner

import org.scalatest.FunSuite

class CedricTest extends FunSuite {
  val test = """
    ""
    "this \"String"
    '\''"""
    
	/*test("add line numbers") {
	  	println(Scanner.addLineNumbers(test))
	}*/
  
	test("Let's try to remove comments") {
	    println("CODE:")
	    println(test)
	    val noComments = Scanner.removeCommentsExtractStrings(test)
	    println("NO COMMENTS:")
	    noComments.foreach(println(_)) 
	    val withSpace = noComments.map(Scanner.addSpace(_))
	    println()
	    println("WITH SPACE:")
	    withSpace.foreach(println(_))
	    val finished = withSpace.flatMap(Scanner.splitCode(_))
	    println()
	    println("ALL DONE:")
	    finished.foreach(println(_))
	}
	
	/*test("Here we go for the categorization"){
	    assert (Scanner.checkEncoding(test))
	    try{
	    Scanner.categorize(Scanner.generateTokens(test)).foreach(println)
	    }catch{
	        case TokenException(eM, _) => println(eM) 
	                fail()
	    }
	}*/
}