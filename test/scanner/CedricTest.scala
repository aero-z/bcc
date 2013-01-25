package scanner

import org.scalatest.FunSuite

class CedricTest extends FunSuite {
	test("Let's try to remove comments"){
	  val test = """
		some(code);
	    while(happy>cedric){dont annoy me}
	    /*multi line comment
	    comment
	    //with non sense comment
	    "and even a string" ; 
	    and also wrong /* comment
	    
	    
	    formation */
	    
	    (more*code)-all.over.the.place
	    a //one line comment
	    A "string thing //with a useless comment"
	    "and more string /*thing which do not make send*/ look"
	    happy.code(takes(me.home))
      """
	  val dbComments = test.replaceAll("""(".*?")""", """$1deadbeef""")
    println("DIRTYHACK:")
    println(dbComments)
    val noComments = Scanner.removeCommentsExtractStrings(dbComments)
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
}