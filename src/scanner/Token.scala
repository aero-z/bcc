package scanner

import parser.Symbol

class Token(str: String) extends Symbol


/**
 * Java keyword tokens
 */
case class KeywordToken(str: String) extends Token(str)

/**
 * Identifiers like fields and method names
 */
case class IdentifierToken(str: String) extends Token(str)

/**
 * Separators token
 */

class SeparatorToken(str: String) extends Token(str)

/**
 * Tokens for scoping purposes, like { and (
 */
case class ScopingToken(str: String) extends SeparatorToken(str) 

case class SemiColonToken(str: String) extends SeparatorToken(str)


/**
 * Represents literals like string and number constants
 */
class LiteralToken(str: String) extends Token(str) 


case class IntegerToken(str : String) extends LiteralToken(str)

case class StringToken(str : String) extends LiteralToken(str)

case class OperatorToken(str: String) extends Token(str)

case class EndToken() extends Token("$")//TODO talk about that


case class TokenException(errorMsg: String, cause : String) extends Exception(errorMsg)
