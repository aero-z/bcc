package parser

import scanner.Token
import scanner.OperatorToken
import scanner.IdentifierToken

trait Symbol{
    def printDot(): Unit
}

case class NonTerminalSymbol(str: String, reducedSymbol: List[Symbol]) extends Symbol {
    def printDot(): Unit ={
        println(s"""${hashCode.toString}[label="$str"]""")
        reducedSymbol.reverse.foreach(x =>{println(s"""${hashCode.toString}->${x.hashCode.toString}"""); x.printDot})
    }
}
