package scanner

import parser.Symbol

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
 * Separators token
 */

abstract class SeparatorToken extends Token

/**
 * Tokens for scoping purposes, like { and (
 */
case class ScopingToken(str: String) extends SeparatorToken {
  val typeStr = str
}

case class SemiColonToken() extends SeparatorToken {
  val typeStr = ";"
}


/**
 * Represents literals like string and number constants
 */
class LiteralToken extends Token {
  val typeStr = "PrimitiveType"
}

case class IntegerToken(int: Integer) extends LiteralToken

case class StringToken(str: String) extends LiteralToken

case class OperatorToken(str: String) extends Token {
  val typeStr = str
}

case class EndToken() extends Token {
  val typeStr = "$"
}

case class TokenException(errorMsg: String, cause: String) extends Exception(errorMsg)
