package parser
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
  def visit(node:IfNode) {}
}
*/

trait AST {
	val root:Node
}

trait Node {
  def accept()
  override def toString():String
}
case class EmptyNode() extends Node {
  def toString = ""
}
case class EndNode(info:String) extends Node
case class BlockNode(list:List[Node]) extends Node
case class AssignmentNode(leftNode:Node, rightNode:Node) extends Node
case class IfNode(condition:Node, thenblock:Node, elseBlock:Node) extends Node {
  def toString = "if ( "+condition.toString+" ) "+thenblock.toString()+" "+elseBlock.toString()
}
case class WhileNode(condition:Node, block:Node) extends Node {
  def toString() = "while ( "+condition.toString+" ) "+block.toString 
}
case class OperationNode(leftSide:Node, operator:String, rightSide:Node) extends Node

object AST {
	val ifstatement = "(IfThenElseStatement|IfThenElseStatementNoShortIf)".r //the same after parsing
	val whilestatement = "(WhileStatement|WhileStatementNoShortIf)".r
	val operations = "(MultiplicativeExpression|AdditiveExpression|RelationalExpression|EqualityExpression|CondOrExpression|CondAndExpression)".r
	def toNode(symbol:Symbol):Node = symbol match {
	  case NonTerminalSymbol("IfThenStatement", list) => IfNode(toNode(list(3)), toNode(list(5)), EmptyNode())
	  case NonTerminalSymbol(ifstatement(_), list) => IfNode(toNode(list(3)), toNode(list(5)), toNode(list(7)))
	  case NonTerminalSymbol(whilestatement(_), list) => WhileNode(toNode(list(3)), toNode(list(5)))
	  case NonTerminalSymbol("Assignment", list) => AssignmentNode(toNode(list(1)), toNode(list(3)))
	  case NonTerminalSymbol(operations(_), List(_,_,_)) => OperationNode(toNode(list(1)), list(2), toNode(list(3)))
	  case NonTerminalSymbol(name, list) => BlockNode(list.map(toNode(_)))
	  case x => EndNode(x.toString())
	  case stuff => EndNode(stuff.toString())
	}
    
	def reducdeSimpleBranches(node:Node):Node = node match {
	  case BlockNode(List(_)) =>
	  case BlockNode(x) => BlockNodes(x.map(reduceSimpleBranches(_)))
	}
	
    def reducdeSimpleSymbolBranches(symbol:Symbol):Symbol = symbol match {
      case NonTerminalSymbol(name, list) =>
        val newlist = list.map(reducdeSimpleSymbolBranches(_))
        if (newlist.length == 1) newlist.head
        else NonTerminalSymbol(name, List(_, _, _))
      case x => x
    }
}