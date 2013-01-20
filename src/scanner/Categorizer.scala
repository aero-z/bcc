package scanner

object Categorizer {
  
  def makeTokens(l:List[String]) = {
    l.map(s => KeywordToken)
  }

}