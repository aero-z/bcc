package ast

import scala.language.implicitConversions
import main.CompilerError
import ast.Modifier._

sealed abstract class CheckResult(passed: Boolean, errStr: String) {
  def ++(c: CheckResult) = {
    if (passed) c
    else this
  }
}

object CheckResult {
  def apply(passed: Boolean, errStr: String): CheckResult =
    if (passed) CheckOk()
    else CheckFail(errStr)
}

case class CheckFail(errStr: String) extends CheckResult(false, errStr)
case class CheckOk() extends CheckResult(true, null)

trait AstNode {
  lazy val weedResult: CheckResult = CheckOk()
  
  implicit def option2List[A](o: Option[A]) = o match {
    case Some(x) => x :: Nil
    case None => Nil
  }
  
  def mergeCheck(x: (Boolean, String), y: (Boolean, String)) = {
    if (x._1) x
    else y
  }
      
  {
    weedResult match {
      case CheckOk() =>
      case CheckFail(err) => throw CompilerError(err)
      case null => throw new RuntimeException("weedResult must be early defined")
    }
  }
}