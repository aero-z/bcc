package scanner

class Token(str: String) {

}


/**
 * Java keyword tokens
 */
case class KeywordToken(str: String) extends Token(str: String)

/**
 * Identifiers like fields and method names
 */
case class IdentifierToken(str: String) extends Token(str: String)

/**
 * Separators token
 */

class SeparatorToken(str: String) extends Token(str: String)

/**
 * Tokens for scoping purposes, like { and (
 */
case class ScopingToken(str: String) extends SeparatorToken(str: String) 

case class SemiColonToken(str: String) extends SeparatorToken(str: String)


/**
 * Represents literals like string and number constants
 */
class LiteralToken(str: String) extends Token(str: String) 


case class IntegerToken(str : String) extends LiteralToken(str: String)

case class StringToken(str : String) extends LiteralToken(str: String)