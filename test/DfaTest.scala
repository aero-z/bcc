import parser.Dfa
import scala.io.Source

object DfaTest {

  def main(args: Array[String]): Unit = {
    Dfa.fromFile(Source.fromFile("cfg/simple.lr1"))
  }

}