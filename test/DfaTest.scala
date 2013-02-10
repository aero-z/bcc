import parser.Dfa
import scala.io.Source

object DfaTest {

  def main(args: Array[String]): Unit = {
    val dfa = Dfa.fromFile(Source.fromFile("cfg/simple.lr1"))
    dfa.delta(dfa.q0, ???)
  }

}