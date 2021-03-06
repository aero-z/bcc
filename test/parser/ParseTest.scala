package parser

import org.scalatest.FunSuite
import scala.io.Source
import scanner.IdentifierToken
import scanner.AssignmentToken
import scanner.IntegerToken
import scanner.OperatorToken
import scanner.EndToken
import scanner.AssignmentToken
import scanner.IntegerToken
import scanner.OperatorToken
import scanner.IntegerToken
import scanner.IdentifierToken
import scanner.ScopingToken


class ParseTest extends FunSuite {
  test("simple grammar test") {
    val dfa = Dfa.fromFile(Source.fromString(
"""6
identifier
integerLiteral
+
;
=
EOF
5
S
expression
block
statement
assignment
S
9
S block EOF
expression identifier
expression integerLiteral
expression expression + integerLiteral
expression expression + identifier
block statement block
block statement
statement assignment ;
assignment identifier = expression
15
31
6 + reduce 1
2 = shift 1
11 EOF reduce 6
11 identifier shift 2
14 + reduce 4
7 + shift 3
11 block shift 4
6 ; reduce 1
1 integerLiteral shift 5
10 + reduce 3
14 ; reduce 4
1 identifier shift 6
8 identifier reduce 7
7 ; reduce 8
1 expression shift 7
4 EOF reduce 5
5 + reduce 2
10 ; reduce 3
13 ; shift 8
0 identifier shift 2
0 block shift 9
5 ; reduce 2
12 identifier reduce 0
3 integerLiteral shift 10
11 statement shift 11
0 statement shift 11
8 EOF reduce 7
9 EOF shift 12
11 assignment shift 13
0 assignment shift 13
3 identifier shift 14
"""
        ))
    val tokens = List(IdentifierToken("x"), AssignmentToken(), IntegerToken("5"), ScopingToken(";"),
        IdentifierToken("y"), AssignmentToken(), IdentifierToken("x"), OperatorToken("+"), IntegerToken("2"), ScopingToken(";"),
        EndToken)
    val parseTree = Parser.parse(tokens, dfa)
    Parser.printTree(parseTree)
    parseTree match {
      case NonTerminalSymbol("block",
        NonTerminalSymbol("statement",
          NonTerminalSymbol("assignment",
            IdentifierToken("x") ::
            AssignmentToken() ::
            NonTerminalSymbol("expression",
              IntegerToken("5") ::
              Nil) ::
            Nil) ::
          ScopingToken(";") ::
          Nil) ::
        NonTerminalSymbol("block",
          NonTerminalSymbol("statement",
            NonTerminalSymbol("assignment",
              IdentifierToken("y") ::
              AssignmentToken() ::
              NonTerminalSymbol("expression",
                NonTerminalSymbol("expression",
                  IdentifierToken("x") ::
                  Nil) ::
                OperatorToken("+") ::
                IntegerToken("2") ::
                Nil) ::
              Nil) ::
            ScopingToken(";") ::
          Nil) ::
        Nil) ::
      Nil) => 
      case _ => fail
    }
  }
}
