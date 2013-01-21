package scanner

object Scanner {
	
    
	def checkEncoding(code: String) = code.toList.forall(ch => ch < 128 && ch >= 0)
	
	val identifiers = """[a-zA-Z$_]([a-zA-Z0-9$_])*"""












}