package scanner

class Token(str: String) {

}

case class KeywordToken(str: String) extends Token(str: String) {
  
}

case class IdentifierToken(str: String) extends Token(str: String) {
  
}

case class ScopingToken(str: String) extends Token(str: String) {
  
}

case class LiteralToken(str: String) extends Token(str: String) {
  
}