package scanner

class Token(str: String) {

}

/**
 * Java keyword tokens
 */
case class KeywordToken(str: String) extends Token(str: String) {
  
}

/**
 * Identifiers like fields and method names
 */
case class IdentifierToken(str: String) extends Token(str: String) {
  
}

/**
 * Tokens for scoping purposes, like { and (
 */
case class ScopingToken(str: String) extends Token(str: String) {
  
}

/**
 * Represents literals like string and number constants
 */
case class LiteralToken(str: String) extends Token(str: String) {
  
}