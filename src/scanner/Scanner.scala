package scanner

object Scanner {

    def checkEncoding(code: String) ={
        code.
        matches("\\p{ASCII}*")
    }



    def generateTokens(code:String):List[String] = {
        removeCommentsExtractStrings(code) //
        .toList
        .map(addSpace(_)) //add whitespace around special characters
        .flatten(splitCode(_)) //split at places indicated by separator-token
    }

    def removeCommentsExtractStrings(code:String):List[String] = {
        /*
         * (?<=".*?")|(?=".*?") separate string groups without rejecting them -> set empty string as splitter
         * ?: is for non capturing groups
         * .* is for any character except line breaks
         * (?m)$ matches the end of the line ($ just matches the end of the input)
         * (?s). matches everything including line breaks
         * * is the greedy version (maximum match)
         * *? is the reluctant (non-greedy) version (minimum match)
         */
        code.replaceAll("""(".*?")""", """$1dirtyhack""").split("""(?=(".*?"))|((?<=")dirtyhack)|(//.*(?m)$)|(/\*(?s).*?\*/)""").toList //((?<=".*?")|(?=".*?"))|//.*(?m)$|(?:/\*(?s).*?\*/)
    }

    def isString(line:String):Boolean = {
        line.matches("""(".*")""")
    }

    def addSpace(line:String): String = {
        //add space around special characters so they are easier to parse
        //this could be done in a cleaner way using "lookahead" and "lookbehind"
        if (isString(line)) line
        else line.replaceAll("""(\+\+|\-\-|<=|>=|==|!=|\|\||[\+\-\*/^\|&?!=<>\(\)\[\]\{\}\.])""", """ $1 """)//Miss the comma and semicolon
    }

    def splitCode(line:String):List[String] = {
        //sperate code at whitespaces
        if (isString(line)) List(line)
        else line.replaceAll("""(^\s+)|(\s+$)""", "").split("""[\s\t\n\r\f\a\e]+""").filter(_ != "").toList
    }
    /*TODO: maybe needed later
	def addLineNumbers(code:String):String = {
	  code.replaceAll("("," ( ")
	  .replaceAll("."," ( ")
	//  code//.split("""(?m)$""").toList.foldLeft(z:String)((x,y)=>x+y)
	}
     */





    def categorize(list : List[String]): List[Token] = {
        val identifiers = "[a-zA-Z\\$_][a-zA-Z0-9\\$_]*".r;

        val keywords = List("abstract", "assert", "boolean", "break", "byte", "case", "catch",
                "char", "class", "const", "continue", "default", "do", "double", "else", "enum",
                "extends", "final", "finally", "float", "for", "if", "goto", "implements", "import",
                "instanceof", "int", "interface", "long", "native", "new", "package", "private", "protected",
                "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
                "throw", "throws", "transient", "try", "void", "volatile", "while");
                
        val space = "[ \t]+".r;
        val parenthesis = "[\\(\\)\\[\\]\\{\\}]".r;
        val semicolon = ";".r;
        val lineReturn = "\n".r;
                       

        list.map{
            _ match{
                case x if keywords contains x => ???
                case identifiers(id) => ???
                case parenthesis(prt) => ???
            }
        }
    }
}
