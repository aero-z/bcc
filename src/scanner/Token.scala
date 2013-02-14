package scanner

import parser.Symbol
import scala.util.Random

abstract class Token extends Symbol {
  def typeStr(): String
}

/*
object Token {
  def fromString(str:String): Token = str match {
    case "if" => KeywordToken("if")
    case "id" => IdentifierToken("noname")
    case ";" => SemiColonToken(";")
    case "scope" => ???
  }
}
*/

/**
 * Java keyword tokens
 */
case class KeywordToken(keyword: String) extends Token {
  def typeStr() = keyword
}

/**
 * Identifiers like fields and method names
 */
case class IdentifierToken(str: String) extends Token {
  val typeStr = "identifier" 
}

/**
 * Tokens for scoping purposes, like { and (
 */
case class ScopingToken(str: String) extends Token {
  val typeStr = str
}

case class SemiColonToken() extends Token {
  val typeStr = ";"
}

case class AssignmentToken() extends Token {
  val typeStr = "="
}


/**
 * Represents literals like string and number constants
 */
class LiteralToken extends Token {
  val typeStr = "literal"
}

case class IntegerToken(int: Int) extends LiteralToken

case class StringToken(str: String) extends LiteralToken

case class OperatorToken(str: String) extends Token {
  val typeStr = str
}

case class EndToken() extends Token {
  val typeStr = "$"
}

case class TokenException(errorMsg: String, cause: String) extends Exception(errorMsg)
