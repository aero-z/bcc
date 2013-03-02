package scanner

import main.CompilerError
import scala.annotation.tailrec
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
    code.replaceAll("""((".*?[^\\]")|("")|('.*?[^\\]'))|(//.*(?m)$)|(/\*(?s).*?\*/)""", "\0$1\0").split("\0").toList.filter(!_.matches("""(//.*(?m)$)|(/\*(?s).*?\*/)""")) //((?<=".*?")|(?=".*?"))|//.*(?m)$|(?:/\*(?s).*?\*/)
  }

  def isString(line: String): Boolean = {
    line.matches("""(".*")|('.*')""")
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

    /*def unescape(str: String) = {
      try {
        StringEscapeUtils.unescapeJava(str)
      } catch {
        case e: NumberFormatException => throw new CompilerError(s"Invalid escape sequence '$str': "+e.getMessage())
      }
    }*/
    
    def unescape(str: String): String = {      
      @tailrec
      def unescapeRec(str: List[Char], acc : String): String = {
        str match {
          case Nil =>  acc
          case '\\' :: 'b'  :: xs => unescapeRec(xs, acc + '\b')
          case '\\' :: 'n'  :: xs => unescapeRec(xs, acc + '\n')
          case '\\' :: 'r'  :: xs => unescapeRec(xs, acc + '\r')
          case '\\' :: 't'  :: xs => unescapeRec(xs, acc + '\t')
          case '\\' :: 'f'  :: xs => unescapeRec(xs, acc + '\f')
          case '\\' :: '''  :: xs => unescapeRec(xs, acc + '\'')
          case '\\' :: '\"' :: xs => unescapeRec(xs, acc + '\"')
          case '\\' :: '\\' :: xs => unescapeRec(xs, acc + '\\')
          case '\\' :: (n1 @ ('0' | '1' | '2' | '3')) ::
		               (n2 @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7')) ::
		               (n3 @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7')) :: xs =>
            unescapeRec(xs, acc + Integer.valueOf("" + n1 + n2 + n3, 8).toChar)
          case '\\' :: (n1 @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7')) ::
               	       (n2 @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7')) :: xs =>
            unescapeRec(xs, acc + Integer.valueOf("" + n1 + n2, 8).toChar)
          case '\\' :: (n1 @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7')) :: xs =>
            unescapeRec(xs, acc + Integer.valueOf("" + n1, 8).toChar)
          case '\\' :: xs => throw new CompilerError("invalid escape sequence "+xs)
          case x :: xs => unescapeRec(xs, acc + x)
        }
      }
      
      unescapeRec(str.toList, "")
    }
            
    
    val identifiers = "([a-zA-Z\\$_][a-zA-Z0-9\\$_]*)".r;

    val keywords = List("abstract", "assert", "boolean", "break", "byte", "case", "catch",
      "char", "class", "const", "continue", "default", "do", "double", "else", "enum",
      "extends", "final", "finally", "float", "for", "if", "goto", "implements", "import",
      "instanceof", "int", "interface", "long", "native", "new", "package", "private", "protected",
      "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
      "throw", "throws", "transient", "try", "void", "volatile", "while");

    val integer = "((?:[1-9][\\d_]*)?\\d)".r;
    val string = "(\"(?s).*\")".r;
    val boolean = "(true|false)".r;
    val char = "('.*')".r;
    val delimiters = List("{", "}", "[", "]", "(", ")", ";", ",", ".");
    val operators = List(">", "<", "!", "==", "<=", ">=", "!=", "&&", "||", "+", "-", "*", "/", "%", "|", "&", "^");
    val assignment = "(=)".r
    list.map {
      _ match {
        case x if keywords contains x => KeywordToken(x)
        case "null" => NullToken()
        case boolean(bool) => BooleanToken(bool.toBoolean)
        case assignment(eq) => AssignmentToken()
        case identifiers(id) => IdentifierToken(id)
        case integer(intlit) => IntegerToken(intlit)
        case string(str) => StringToken(unescape(str.substring(1, str.length() - 1)))
        case char(chr) =>
          val c = unescape(chr.substring(1, chr.length() - 1))
          if (c.length != 1) throw new CompilerError(s"invalid char literal $chr")
          CharacterToken(c)
        case x if delimiters contains x => ScopingToken(x)
        case x if operators contains x => OperatorToken(x)
        case x => throw new CompilerError(s"Cannot categorize $x")
      }
    } :+ EndToken()
  }
}
