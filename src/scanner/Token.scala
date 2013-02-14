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
  def typeStr = keyword
}

/**
 * Identifiers like fields and method names
 */
case class IdentifierToken(str: String) extends Token {
  def typeStr = "identifier" 
}

/**
 * Tokens for scoping purposes, like { and (
 */
case class ScopingToken(str: String) extends Token {
  def typeStr = str
}


case class AssignmentToken() extends Token {
  def typeStr = "="
}


/**
 * Represents literals like string and number constants
 */


case class IntegerToken(int: Int) extends Token {
    def typeStr = "integerLiteral"
}

case class StringToken(str: String) extends Token{
    def typeStr = "stringLiteral"
}

case class BooleanToken(bool: Boolean) extends Token{
    def typeStr = bool.toString
}

case class CharacterToken(char : Char) extends Token{
    def typeStr = "characterLiteral"
}

case class OperatorToken(str: String) extends Token {
  def typeStr = str
}

case class NullToken() extends Token{
    def typeStr = "null"
}

case class EndToken() extends Token {
  val typeStr = "EOF"
}

case class TokenException(errorMsg: String, cause: String) extends Exception(errorMsg)
