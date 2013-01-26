package scanner



object Scanner {


    def checkEncoding(code: String) = code.toList.forall(ch => ch < 128 && ch >= 0)
    def scan(code : String): List[Token] = {
       
        val identifiers = "\\A[a-zA-Z$_]([a-zA-Z0-9$_])*".r
                val keywords = List("abstract", "assert", "boolean", "break", "byte", "case", "catch",
                        "char", "class", "const", "continue", "default", "do", "double", "else", "enum",
                        "extends", "final", "finally", "float", "for", "if", "goto", "implements", "import",
                        "instanceof", "int", "interface", "long", "native", "new", "package", "private", "protected",
                        "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
                        "throw", "throws", "transient", "try", "void", "volatile", "while").
                        map(x => s"(\\A$x)").
                        reduce((x, y) => s"$x|$y").r

                     val space = "\\A[ \t]+".r
                     val parenthesis = "\\A[\\(\\)\\[\\]\\{\\}]".r
                     val semicolon = "\\A;".r
                     val lineReturn = "\\A\n".r
                        def scanAcc(code: String, acc: List[Token]): List[Token] = {
            println(acc)
            code match{
                            case space(s) => scanAcc(code.replaceFirst(s, ""), acc)
                            case parenthesis(s) => scanAcc(code.replaceFirst(s, ""), ScopingToken(s)::acc)
                            case lineReturn(s) => scanAcc(code.replaceFirst(s, ""), LineReturnToken(s)::acc)
                            case semicolon(s) => scanAcc(code.replaceFirst(s, ""), SemiColonToken(s):: acc)
                            case keywords(s) => scanAcc(code.replaceFirst(s, ""), KeywordToken(s)::acc)
                            case identifiers(s) => scanAcc(code.replaceFirst(s, ""), IdentifierToken(s)::acc)
                            case x => println(x)
                            ???
        }}
        scanAcc(code, Nil).reverse
    }







}