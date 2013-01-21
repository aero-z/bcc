package scanner

object Categorizer {
  
	def makeTokens(l:List[String]) : List[Token] = {
	  // this is all untested crazy stuff!!!
		val h = ".*".r
		l.map(_ match {
			case h => LiteralToken("hoi")
			case ".*" => LiteralToken("i")
		})
	}

	
    def main(args: Array[String]) {
      val l = List("if", "(", "x", "==", "3", ")", "{", "hello", "++", ";", "}")
	  makeTokens(l).foreach(println _)
	}
}