package typecheck

import org.scalatest.FunSuite
import scala.io.Source
import main.Joosc

class TypeTest extends FunSuite {

  test("constructor test") {
    val code = List(
       ("""
        package A;
        public class Hoi {
           protected Hoi() {}
        }
        """, "Hoi.java"),
        ("""
        package B;
        import A.Hoi;    
        
        public class Hello extends Hoi {
           public Hello() {}
        }
        """, "Hello.java")
    ).map(s => (Source.fromString(s._1), s._2))
  
    assert(Joosc.compile(code) == main.Joosc.errCodeSuccess)
  }
}
