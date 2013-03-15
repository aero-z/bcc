package ast

import scala.language.implicitConversions
import main.CompilerError
import ast.Modifier._

/**
 * return value of weeding check
 */
sealed abstract class WeedResult(passed: Boolean, errStr: String) {
  /**
   * merge two results to one result (only success if both are successful)
   */
  def ++(c: WeedResult) = {
    if (passed) c
    else WeedResult.this
  }
}

case class WeedFail(errStr: String) extends WeedResult(false, errStr)
case class WeedOk() extends WeedResult(true, null)

/**
 * Helper object to create WeedResult instances
 */
object WeedResult {
  def apply(passed: Boolean, errStr: String): WeedResult =
    if (passed) WeedOk()
    else WeedFail(errStr)
}

/**
 * Every node in the AST implements this trait
 */
trait AstNode {
  lazy val weedResult: WeedResult = WeedOk()
      
  {
    weedResult match {
      case WeedOk() =>
      case WeedFail(err) => throw CompilerError(err)
      case null => throw new RuntimeException("weedResult must be early defined")
    }
  }
}