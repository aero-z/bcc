package parser

import scanner.Token
import scanner.KeywordToken

object Weeder {
  def check(s: Symbol): Boolean = {
    /**
     * @param tree a Modifiers or Modifier tree
     * @return total number of final and abstract modifiers
     */
    def countFinalAbstract(tree: NonTerminalSymbol): Int = {
      tree match {
        case NonTerminalSymbol("Modifiers", (newmods @ NonTerminalSymbol("Modifiers",_)) :: (mod @ NonTerminalSymbol("Modifier",_)) :: Nil) =>
          countFinalAbstract(newmods) + countFinalAbstract(mod)
        case NonTerminalSymbol("Modifiers", (mod @ NonTerminalSymbol("Modifier",_)) :: Nil) =>
          countFinalAbstract(mod)
        case NonTerminalSymbol("Modifier", KeywordToken(keyword) :: Nil) => keyword match {
	      case "final" | "abstract" => 1
	      case _ => 0
          }
        case _ => throw new RuntimeException("bad parse tree")
      }
    }
    def checkClassModifiers(x: NonTerminalSymbol) = (countFinalAbstract(x) <= 1)
    s match {
      case NonTerminalSymbol("ClassDeclaration", (mods @ NonTerminalSymbol("Modifiers", _)) :: _) => checkClassModifiers(mods)
      case NonTerminalSymbol("InterfaceDeclaration", (mods @ NonTerminalSymbol("Modifiers", _)) :: _) => checkClassModifiers(mods)
      case NonTerminalSymbol(_, xs) => xs.map(check(_)).reduce(_ && _)
      case _: Token => true
    }
  }

}