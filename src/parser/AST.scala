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

class Node {
  def accept()
}
case class EmptyNode() extends Node
case class EndNode(info:String) extends Node
case class BlockNode(list:List[Node]) extends Node
case class Assignment(leftNode:Node, rightNode:Node) extends Node
case class IfNode(condition:Node, thenblock:Node, elseblock:Node) extends Node
case class WhileNode(condition:Node, block:Node) extends Node

object AST {
	def toNode(symbol:Symbol):Node = symbol match {
	  case NonTerminalSymbol("IfThenStatement", list) => IfNode(toNode(list(3)), toNode(list(5)), EmptyNode())
	  case NonTerminalSymbol("IfThenStatement", list) => IfNode(toNode(list(3)), toNode(list(5)), toNode(list(7)))
	  case NonTerminalSymbol(name, list) => BlockNode(list.map(toNode(_)))
	    
	  case x => EndNode(x.toString())
	}
    
    def reducdeSimpleBranches(symbol:Symbol):Symbol = symbol match {
      case NonTerminalSymbol(name, list) =>
        val newlist = list.map(reducdeSimpleBranches(_))
        if (newlist.length == 1) newlist.head
        else NonTerminalSymbol(name, newlist)
      case x => x
    }
}