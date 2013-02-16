package parser

import scanner._
import parser._
/*
trait Visitor {
  def visit(node:AbstractNode) {
    node.accept(this)
  }
}

trait AbstractNode {
  def accept(visitor:Visitor) {
    visitor.visit(this)
  }
}

class TypeChecking extends (Visitor) {
  def visit(node:IfNode) {}
}*/
/*
trait AST {
	//val root
}

trait Node {
 //def accept()
  override def toString():String
}
case class EmptyNode() extends Node {
  override def toString() = ""
}
case class EndNode(info:String) extends Node
case class BlockNode(list:List[Node]) extends Node {
  override def toString() = list.foldLeft("")((str, node) => str+" "+node.toString)
}

case class AssignmentNode(leftNode:Node, rightNode:Node) extends Node {
  override def toString() = leftNode.toString+" = "+rightNode.toString
}
case class IfNode(condition:Node, thenblock:Node, elseBlock:Node) extends Node {
  override def toString() = "if ( "+condition+" ) "+thenblock+" "+elseBlock
}
case class WhileNode(condition:Node, block:Node) extends Node {
  override def toString() = "while ( "+condition.toString+" ) "+block.toString 
}
case class OperationNode(leftSide:Node, operator:String, rightSide:Node) extends Node {
  override def toString() = leftSide.toString+" "+operator+" "+rightSide.toString
}
case class CompareExpressionNode(leftSide:Node, operator:String, rightSide:Node) extends Node {
  override def toString() = leftSide.toString+" "+operator+" "+rightSide.toString
}
*/
//case class ClassDeclaration(modifiers:BlockNode, identifier:String, interfaces:BlockNode, classBodyDeclarations:BlockNode)

  object Ast {
   /*
	val ifstatement = "(IfThenElseStatement|IfThenElseStatementNoShortIf)".r //the same after parsing
	val whilestatement = "(WhileStatement|WhileStatementNoShortIf)".r
	val operations = "(MultiplicativeExpression|AdditiveExpression|RelationalExpression|EqualityExpression|CondOrExpression|CondAndExpression)".r
	val comparator = "(RelationalExpression|EqualityExpression)".r
	def toNode(symbol:Symbol):Node = symbol match {
	  case NonTerminalSymbol("IfThenStatement", list) => IfNode(toNode(list(2)), toNode(list(4)), EmptyNode())
	  case NonTerminalSymbol(ifstatement(_), list) => IfNode(toNode(list(2)), toNode(list(4)), toNode(list(7)))
	  case NonTerminalSymbol(whilestatement(_), list) => WhileNode(toNode(list(2)), toNode(list(4)))
	  case NonTerminalSymbol("Assignment", list) => AssignmentNode(toNode(list(0)), toNode(list(2)))
	  case NonTerminalSymbol(operations(_), x :: y :: z :: Nil) => OperationNode(toNode(x), y.toString, toNode(z))
	  case NonTerminalSymbol(comparator(_), x::(y:Token)::z::Nil) => CompareExpressionNode(toNode(x), y.toString, toNode(y))
	  
	  case NonTerminalSymbol("ClassDeclaration",
	      NonTerminalSymbol("Modifiers", modifiers)::
	      KeywordToken("class")::
	      KeywordToken("extends")::
	      (classid @ IdentifierToken(_))::
	      KeywordToken("implements")::
	      (typeid @ IdentifierToken(_))::
	      ScopingToken("{")::
	      NonTerminalSymbol("ClassBodyDeclarations", classbodydeclarations)::
	      ScopingToken("}")::Nil)
	      => ClassDeclaration(toNode(modifiers), classid, interfaces:BlockNode, classBodyDeclarations:BlockNode)
ClassDeclaration Modifiers class identifier extends Type implements Interfaces { ClassBodyDeclarations }
ClassDeclaration Modifiers class identifier implements Interfaces { ClassBodyDeclarations }
ClassDeclaration Modifiers class identifier extends Type { ClassBodyDeclarations }
ClassDeclaration Modifiers class identifier { ClassBodyDeclarations }
ClassDeclaration class identifier extends Type implements Interfaces { ClassBodyDeclarations }
ClassDeclaration class Type implements Interfaces { ClassBodyDeclarations }
ClassDeclaration class identifier extends Type { ClassBodyDeclarations }
ClassDeclaration class identifier { ClassBodyDeclarations }
ClassDeclaration Modifiers class identifier extends Type implements Interfaces { }
ClassDeclaration Modifiers class identifier implements Interfaces { }
ClassDeclaration Modifiers class identifier extends Type { }
ClassDeclaration Modifiers class identifier { }
ClassDeclaration class identifier extends Type implements Interfaces { }
ClassDeclaration class Type implements Interfaces { }
ClassDeclaration class identifier extends Type { }
ClassDeclaration class identifier { }  

	  
	  case NonTerminalSymbol(name, list) => BlockNode(list.map(toNode(_)))
	  case x:Token => EndNode(x.toString)
	  case x => EndNode(x.toString()) //Bad idea!
	}
    
	def reduceSimpleBranches(node:Node):Node = node match {
	  case BlockNode(head :: Nil) => reduceSimpleBranches(head)
	  case BlockNode(list) => BlockNode(list.map(reduceSimpleBranches(_)))
	}
	
	def treeListExpand(node:Node):Node = {
	  def treeListExpandRec(node:Node):List[Node] = node match {
	    case BlockNode(list) => list.flatMap(treeListExpandRec(_))
	    case x:Node => List(x)
	  }
	  node match {
	  	case BlockNode(list) => BlockNode(list.flatMap(treeListExpandRec(_)))
	  	case x => x
	  }
	}
	
	def treeListExpand(symbol:Symbol):Node = {
	  def treeListExpandRec(symbol:Symbol):List[Node] = symbol match {
	    case NonTerminalSymbol(name, list) => list.flatMap(treeListExpandRec(_))
	    case x:Token => List(EndNode(x.toString))
	  }
	  symbol match {
	  	case NonTerminalSymbol(name, list) => BlockNode(list.flatMap(treeListExpandRec(_)))
	  	case x => EndNode(x.toString())
	  }
	}
	
	  def printASTRec(delim:String, node:Node) {
	  	node match {
	  	    case BlockNode(list) => list.foreach(printASTRec(delim+" ", _))
	  	    case node:Node => println(delim+node.toString)
	  	}
	  }*/ 
	
    def createAst(symbol:Symbol):Symbol = {
      parametersToList(reduceSimpleSymbolBranches(symbol))
    }
    val recursiv = List("Modifiers", "Interfaces", "ClassBodyDeclarations", "ImportDeclarations", "TypeDeclarations", "InterfaceBodyDeclarations", "ParameterDefs", "Parameters", "Statements", "TypeDeclarations") 

    val blackList = List("CompilationUnit")
    def reduceSimpleSymbolBranches(symbol:Symbol):Symbol = symbol match {
      case NonTerminalSymbol(name , list) =>
        val newlist = list.map(reduceSimpleSymbolBranches(_))
        if (newlist.length == 1 && ! blackList.contains(name) && ! recursiv.contains(name)) newlist.head
        else NonTerminalSymbol(name, newlist)
      case x => x
    }
    
    def parametersToList(symbol:Symbol):Symbol = symbol match {
      case NonTerminalSymbol(name, tree) if recursiv.contains(name)=> NonTerminalSymbol(name, listFromTree(tree))
      case NonTerminalSymbol(name, list) => NonTerminalSymbol(name, list.map(parametersToList(_)))
      case x:Symbol => x
    }
    
    def listFromTree(tree:List[Symbol]):List[Symbol] = {
      def listFromSymbol(symbol:Symbol):List[Symbol] = symbol match {
        case NonTerminalSymbol(name, list) => list.flatMap(listFromSymbol(_))
    	case x:Symbol => List(x)
      }
    	tree.flatMap(listFromSymbol(_))
    }
}
