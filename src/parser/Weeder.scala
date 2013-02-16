package parser

import scanner.Token
import scanner.KeywordToken
import scanner.ScopingToken

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
  
  def astcheck(ast:Symbol): Boolean = {
    recCheck(ast) //&& everyNode(ast)
  }
  /*
  def everyNode(ast:Symbol):Boolean = {
    def
    ast match = {}
* check int range//case NonTerminalSymbol(
  }*/
    
  def recCheck(ast:Symbol): Boolean = {
    def checkRec(ast:Symbol):Boolean = ast match {
      case NonTerminalSymbol("ClassDeclaration", NonTerminalSymbol("Modifiers", modlist) :: list) =>
        //A class cannot be both abstract and final
        if ( modlist.contains(KeywordToken("abstract")) && modlist.contains(KeywordToken("final"))) false
        //Every class must contain at least one explicit constructor.
        else !findConstructor(getClassBody(list))
        
      case NonTerminalSymbol("MethodDeclaration", NonTerminalSymbol("Modifiers", modlist) :: tail) =>
        //method has a body if and only if it is neither abstract nor native
        if ( (modlist.contains(KeywordToken("abstract")) || modlist.contains(KeywordToken("native"))) ) 
          tail(tail.length - 1) match {
          	case NonTerminalSymbol("Block", _) => println("END: "+tail(tail.length - 1)); false
          	case _ =>println("END: "+tail(tail.length - 1)); true
          }
        //An abstract method cannot be static or final
        else if (modlist.contains(KeywordToken("abstract")) && (modlist.contains(KeywordToken("static")) || modlist.contains(KeywordToken("final"))) ) false
        //A static method cannot be final
        else if (modlist.contains(KeywordToken("static")) && modlist.contains(KeywordToken("static"))) false
        //A native method must be static.
        else if (modlist.contains(KeywordToken("native")) && !modlist.contains(KeywordToken("static"))) false
        
        else true
        
      //No field can be final.
      case NonTerminalSymbol("SingleVariableDeclaration", NonTerminalSymbol("Modifiers", modlist) :: xs ) =>
        if (modlist.contains(KeywordToken("final")) && xs.length<4) false // "<4" -> means no assignment => Bastien did it too
        else true
        
      //An interface cannot contain fields or constructors.
      case NonTerminalSymbol("InterfaceDeclaration", interfaceDecl) => interfaceCheck(interfaceDecl)
      
      case _ => true
    }
    ast match {
      case nt @ NonTerminalSymbol(_, list) => checkRec(nt) && list.map(astcheck(_)).reduce(_ && _)
      case _ => true
    }
  }
  
  def getClassBody(list:List[Symbol]):List[Symbol] = list match{
    case ScopingToken("{") :: ScopingToken("}") :: xs => Nil
    case ScopingToken("{") :: NonTerminalSymbol("ClassBodyDeclarations", list) :: tail => list
    case x :: tail => getClassBody(tail)
  }
  def findConstructor(list:List[Symbol]):Boolean = {
    def rec(symbol:Symbol): Boolean = symbol match {
      case NonTerminalSymbol("ConstructorDeclaration", list) => true
      case _ => false
    }
    list.map(rec(_)).reduce(_ || _)
  }
  
  //true -> 
  def interfaceCheck(interfaceDecl:List[Symbol]):Boolean = {
    def getInterfaceBody(interfaceDecl:List[Symbol]):List[Symbol] = interfaceDecl match {
    	case ScopingToken("{") :: ScopingToken("}") :: xs => Nil
    	case ScopingToken("{") :: NonTerminalSymbol("InterfaceBodyDeclarations", list) :: tail => list
    	case x :: tail => getInterfaceBody(tail)
    }
    def methodHasBody(methodDecl:List[Symbol]):Boolean = methodDecl match {
      case NonTerminalSymbol("Block", list) :: _ => false
      case _ :: methodDecl => methodHasBody(methodDecl)
      case Nil => true
    }
    def rec(symbol:Symbol):Boolean = symbol match {
      case NonTerminalSymbol("MethodDeclaration", methlist) => methlist match {
    	  //An interface method cannot be static, final, or native.
        case NonTerminalSymbol("Modifiers", modlist) :: methodDecl =>
          if ( modlist.contains(KeywordToken("static")) || modlist.contains(KeywordToken("final")) || modlist.contains(KeywordToken("native")) ) false
        //An interface method cannot have a body.
          else methodHasBody(methodDecl)
        case methodDecl => methodHasBody(methodDecl)
      }
      case _ => true
    }
    getInterfaceBody(interfaceDecl).map(rec(_)).reduce(_ && _)
  }
  
  /*

A formal parameter of a method must not have an initializer.
->BAstien A class/interface must be declared in a .java file with the same base name as the class/interface.

No multidimensional array types or array creation expressions are allowed.
A method or constructor must not contain explicit this() or super() calls.
* 
* cast not in double (( ))
* check char -> count = 1
   */

}