package scanner

class Token(str: String)

object Token {
  def fromString(str:String): Token = str match {
    case "keyword" => KeywordToken("noname")
    case "identifier" => IdentifierToken("noname")
    case "separator" => SeparatorToken("noname")
    case "scope" => ...
  }
}

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

case class EndToken() extends Token("$")


case class TokenException(errorMsg: String, cause : String) extends Exception(errorMsg)
