package scanner

import org.scalatest.FunSuite

class ScannerRegex extends FunSuite {
//    test("Let's try to remove comments"){
//        val helloWithComment = 
//"""public class HelloWorld {
////Say hello
//    public static void main(String[] args) {
//        System.out.println("Hello, World");
//    }
//}"""
//       val helloWithoutComment =
//"""public class HelloWorld {
//
//    public static void main(String[] args) {
//        System.out.println("Hello, World");
//    }
//}"""
//       //assert( Scanner.deleteComments(helloWithComment)== helloWithoutComment)
//    }
    
    
    test("Scanning"){
       val simplest =
           """ public class Simple {
           int rouge;
           Simple(int rouge){
               rouge;
           }
       }"""
           assert(Scanner.checkEncoding(simplest))
    }
}
