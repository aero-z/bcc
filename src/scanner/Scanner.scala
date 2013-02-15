package scanner

import main.CompilerError

object Scanner {

    def scan(code: String): List[Token] = {
        if (!checkEncoding(code)) throw new CompilerError("bad encoding") // TODO
        categorize(generateTokens(code))
    }

    def checkEncoding(code: String) = {
        code.
            matches("\\p{ASCII}*")
    }

    def generateTokens(code: String): List[String] = {
        removeCommentsExtractStrings(code) //
            .toList
            .map(addSpace(_)) //add whitespace around special characters
            .flatten(splitCode(_)) //split at places indicated by separator-token
    }

    def removeCommentsExtractStrings(code: String): List[String] = {
        /*
	   * (?<=".*?")|(?=".*?") separate string groups without rejecting them -> set empty string as splitter
	   * ?: is for non capturing groups
	   * .* is for any character except line breaks
	   * (?m)$ matches the end of the line ($ just matches the end of the input)
	   * (?s). matches everything including line breaks
	   * * is the greedy version (maximum match)
	   * *? is the reluctant (non-greedy) version (minimum match)
	   */
        // TODO: this string won't be correctly parsed: "\""
        code.replaceAll("""(".*?")|('.*?')|(//.*(?m)$)|(/\*(?s).*?\*/)""", "\0$1\0").split("\0").toList.filter(!_.matches("""(//.*(?m)$)|(/\*(?s).*?\*/)""")) //((?<=".*?")|(?=".*?"))|//.*(?m)$|(?:/\*(?s).*?\*/)
    }

    def isString(line: String): Boolean = {
        line.matches("""(?:".*")|(?:'.*')""")
    }

    def addSpace(line: String): String = {
        //add space around special characters so they are easier to parse
        //this could be done in a cleaner way using "lookahead" and "lookbehind"
        if (isString(line)) line
        else line.replaceAll("""(\+\+|\-\-|&&|<=|>=|==|!=|\|\||[\+\-\*/^\|&?!=<>\(\)\[\]\{\}\.,;:%])""", """ $1 """)
    }

    def splitCode(line: String): List[String] = {
        //sperate code at whitespaces
        if (isString(line)) List(line)
        else line.replaceAll("""(^\s+)|(\s+$)""", "").split("""[\s\t\n\r\f\a\e]+""").filter(_ != "").toList
    }

    def addLineNumbers(code: String): String = {
        var counter = 0;
        code.replaceAll("((?m)^.*(?m)$)", (counter = counter + 1).toString() + " $1")
    }

    def categorize(list: List[String]): List[Token] = {
        val identifiers = "([a-zA-Z\\$_][a-zA-Z0-9\\$_]*)".r;

        val keywords = List("abstract", "assert", "boolean", "break", "byte", "case", "catch",
            "char", "class", "const", "continue", "default", "do", "double", "else", "enum",
            "extends", "final", "finally", "float", "for", "if", "goto", "implements", "import",
            "instanceof", "int", "interface", "long", "native", "new", "package", "private", "protected",
            "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
            "throw", "throws", "transient", "try", "void", "volatile", "while");

        val integer = "((?:[1-9][\\d_]*)?\\d)".r;
        val string = "(^\"(?s).*\"$)".r;
        val boolean = "(true|false)".r;
        val char = "(^'.*'$)".r;
        val delimiters = List("{", "}", "[", "]", "(", ")", ";", ",", ".");
        val operators = List(">", "<", "!", "==", "<=", ">=", "!=", "&&", "||", "+", "-", "*", "/", "%");
        val assignment = "(=)".r
        list.map{
            _ match{
                case x if keywords contains x => KeywordToken(x)
                case "null" => NullToken()
                case boolean(bool) => BooleanToken(bool.toBoolean)
                case assignment(eq) => AssignmentToken()
                case identifiers(id) => IdentifierToken(id)
                case integer(intlit) => IntegerToken(intlit.toInt)
                case string(str) => StringToken(str.substring(1, str.length() - 1))
                case char(chr) => CharacterToken(chr.charAt(1))
                case x if delimiters contains x => ScopingToken(x)
                case x if operators contains x => OperatorToken(x)
                case x => throw new CompilerError(s"Cannot categorize $x")
            }
        } :+ EndToken()
    }
}
