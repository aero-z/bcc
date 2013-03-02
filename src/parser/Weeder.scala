package parser

import scanner.Token
import scanner.KeywordToken
import scanner.ScopingToken
import scanner.IntegerToken
import scanner.OperatorToken
import abstractSyntaxTree._

object Weeder {
//--All characters in the input program must be in the range of 7-bit ASCII (0 to 127).
//-> done in Scanner
  def check(cu:CompilationUnit):Boolean = cu match {
    case CompilationUnit(packageName, importDeclarations, typeDef, fileName) => {
      val (modifiers, methods) = typeDef match {
        case None => (Nil, Nil)
        case Some(ClassDefinition(_, _, _, modifiers, _, _, methods)) => (modifiers, methods)
        case Some(InterfaceDefinition(_, _, modifiers, methods)) => (modifiers, methods)
      }
      (
        // no duplicate class modifiers
        modifiers.distinct.length == modifiers.length 
          && 
        // not abstract and final at the same time
        !(modifiers.contains(Modifier.abstractModifier) && modifiers.contains(Modifier.finalModifier))
          &&
        // test each method:
        methods.forall(_ match {
          case MethodDeclaration(_, _, modifiers, _, implementation) =>
            (
            // a method has a body if and only if it is neither abstract nor native.
            implementation.isDefined == (!modifiers.contains(Modifier.abstractModifier) && !modifiers.contains(Modifier.nativeModifier))
              &&
            // a static method cannot be final.
            modifiers.contains(Modifier.abstractModifier) == (!modifiers.contains(Modifier.staticModifier) && !modifiers.contains(Modifier.finalModifier))
              &&
            // a native method must be static.
            modifiers.contains(Modifier.nativeModifier) == !modifiers.contains(Modifier.staticModifier)
            )
        })
      )
    }
  }
//A class cannot be both abstract and final.
//A method has a body if and only if it is neither abstract nor native.
//An abstract method cannot be static or final.
//A static method cannot be final.
//A native method must be static.
//The type void may only be used as the return type of a method.
//A formal parameter of a method must not have an initializer.
//A class/interface must be declared in a .java file with the same base name as the class/interface.
//An interface cannot contain fields or constructors.
//An interface method cannot be static, final, or native.
//An interface method cannot have a body.
//Every class must contain at least one explicit constructor.
//No field can be final.
//No multidimensional array types or array creation expressions are allowed.
//A method or constructor must not contain explicit this() or super() calls.

  /*def check(s: Symbol): Boolean = {
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
  }*/
  
  def astcheck(ast:Symbol): Boolean = {
    recCheck(ast) && everyNode(ast)
  }
  def everyNode(ast:Symbol):Boolean = ast match {
      //check int range
      case IntegerToken(str) =>
        try { str.toInt; println("int conversion ok"); true} catch { case _:Throwable =>println("int conversion problem"); false }
      case NonTerminalSymbol("UnaryExpression", OperatorToken("-") :: IntegerToken(str) :: Nil) =>
        try { ("-"+str).toInt; println("minus int conversion ok"); true} catch { case _:Throwable =>println("minus int conversion problem"); false }
      //cast not in double (( ))
      /*case NonTerminalSymbol("NonPrimCast", ScopingToken("(") :: NonTerminalSymbol(_, ScopingToken("(") :: ys) :: xs) =>println("double (( casting"); false
      case NonTerminalSymbol(_, Nil) => true;*/
      case NonTerminalSymbol(_, list) => list.map(everyNode(_)).reduce(_ && _)
      case _ => true
  }
    
  def recCheck(ast:Symbol): Boolean = {
    def checkRec(ast:Symbol):Boolean = ast match {
      case NonTerminalSymbol("ClassDeclaration", NonTerminalSymbol("Modifiers", modlist) :: list) =>
        //A class cannot be both abstract and final
        if ( modlist.contains(KeywordToken("abstract")) && modlist.contains(KeywordToken("final"))) {println("class is abstract and final"); false}
        //Every class must contain at least one explicit constructor.
        
        else findConstructor(getClassBody(list))
        
      case NonTerminalSymbol("MethodDeclaration", NonTerminalSymbol("Modifiers", modlist) :: tail) =>
        //method has a body if and only if it is neither abstract nor native
        if ( (modlist.contains(KeywordToken("abstract")) || modlist.contains(KeywordToken("native"))) ) 
          tail(tail.length - 1) match {
          	case NonTerminalSymbol("Block", _) => println("abstract method has body"); false
          	case _ =>println("END: "+tail(tail.length - 1)); true
          }
        //An abstract method cannot be static or final
        else if (modlist.contains(KeywordToken("abstract")) && (modlist.contains(KeywordToken("static")) || modlist.contains(KeywordToken("final"))) ) {println("method is abstract and (static or final)"); false}
        //A static method cannot be final
        else if (modlist.contains(KeywordToken("static")) && modlist.contains(KeywordToken("final"))) {println("method is static and final"); false}
        //A native method must be static.
        else if (modlist.contains(KeywordToken("native")) && !modlist.contains(KeywordToken("static"))) {println("method is native and static"); false}
        else true
      //No field can be final.
      /*case NonTerminalSymbol("SingleVariableDeclaration", NonTerminalSymbol("Modifiers", modlist) :: xs ) =>
        if (modlist.contains(KeywordToken("final")) && xs.length<4) false // "<4" -> means no assignment => Bastien did it too
        else true
        
      //An interface cannot contain fields or constructors.
      case NonTerminalSymbol("InterfaceDeclaration", interfaceDecl) => interfaceCheck(interfaceDecl)
      */
      case _ => true
    }
    ast match {
      case nt @ NonTerminalSymbol(_, Nil) => true
      case nt @ NonTerminalSymbol(_, list) => checkRec(nt) && list.map(recCheck(_)).reduce(_ && _)
      case _ => true
    }
  }
  
  def getClassBody(list:List[Symbol]):List[Symbol] = list match{
    case ScopingToken("{") :: ScopingToken("}") :: xs => Nil
    case ScopingToken("{") :: NonTerminalSymbol("ClassBodyDeclarations", list) :: tail => list
    case x :: tail => getClassBody(tail)
    case Nil => Nil
  }
  def findConstructor(list:List[Symbol]):Boolean = {
    def rec(symbol:Symbol): Boolean = symbol match {
      case NonTerminalSymbol("ConstructorDeclaration", list) =>println("constructor found"); true
      case _ => false
    }
    list match {
      case Nil => false
      case list => list.map(rec(_)).reduce(_ || _)
    }
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
A method or constructor must not contain explicit this() or super() calls.

   */

}
