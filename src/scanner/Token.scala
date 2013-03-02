package scanner

import parser.ParserSymbol
import abstractSyntaxTree.Expression
abstract class Token extends ParserSymbol {
  def typeStr: String
  override def toString():String = typeStr
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
  override def toString() = str
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


case class IntegerToken(intLit: String) extends Token with Expression {
    def typeStr = "integerLiteral"
    override def toString() = intLit
}

case class StringToken(str: String) extends Token with Expression{
    def typeStr = "stringLiteral"
    override def toString() = str
}

case class BooleanToken(bool: Boolean) extends Token with Expression{
    def typeStr = "booleanLiteral"
}

case class CharacterToken(string : String) extends Token with Expression{
    def typeStr = "characterLiteral"
    override def toString() = string
}

case class OperatorToken(str: String) extends Token {
  def typeStr = str
}

case class NullToken() extends Token with Expression{
    def typeStr = "null"
}

case class EndToken() extends Token {
  val typeStr = "EOF"
}

case class TokenException(errorMsg: String, cause: String) extends Exception(errorMsg)
