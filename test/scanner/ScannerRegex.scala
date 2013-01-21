package scanner

import org.scalatest.FunSuite

class ScannerRegex extends FunSuite{
    test("Let's try to remove comments"){
        val helloWithComment = 
"""public class HelloWorld {
//Say hello
    public static void main(String[] args) {
        System.out.println("Hello, World");
    }
}"""
       val helloWithoutComment =
"""public class HelloWorld {

    public static void main(String[] args) {
        System.out.println("Hello, World");
    }
}"""
       //assert( Scanner.deleteComments(helloWithComment)== helloWithoutComment)
    }
    
    
    test("Identifiers"){
        val identifiers = List("public", "_cedric", "$root$", "r3r3r3r3r3r3r3r3r",
                "MyVariable", "myvariable", "MYVARIABLE", "x", "i", "_myvariable",
                "$myvariable", "_9pins", "andros", "OReilly", 
                "This_is_an_insanely_long_variable_name_that_just_keeps_going_and_going_and_going_and_well_you_get_the_idea_The_line_breaks_arent_really_part_of_the_variable_name_Its_just_that_this_variable_name_is_so_ridiculously_long_that_it_won't_fit_on_the_page_I_cant_imagine_why_you_would_need_such_a_long_variable_name_but_if_you_do_you_can_have_it")
        
        val notid = List("3r3r3r", "My Variable", "9pins", "a+c", "testing1-2-3", "O'Reilly", "OReilly_&_Associates")
        
        assert(identifiers.forall(x => s"\\A${Scanner.identifiers}\\G".r.findFirstIn(x).isDefined))
    }
}
