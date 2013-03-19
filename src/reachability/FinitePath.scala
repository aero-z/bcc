package reachability

import ast._
import main.Logger.debug
import main.Joosc

class ReturnException(message:String) extends main.CompilerError(message, "Return check ")

object FinitePath {
  //MethodDeclaration(
//	methodName: String,
//	returnType: Type,
//	modifiers: List[Modifier],
//	parameters: List[Parameter],
//	implementation: Option[Block])
	def check(m:MethodDeclaration) {
	  def returns(s:Statement):Boolean = s match { //we use Booleans for a lazy evaluation
	    case Block(statements) =>
	      	statements match {
		      case Nil => false
		      case x :: Nil => returns(x)
		      case x :: xs => returns(x) || returns(Block(xs)) //Block = Wrapper
		    }
	    case EmptyStatement =>
	      false
	    case ExpressionStatement(_) =>
	      false //expressions never return anything!
	    case ForStatement(_, _, _, loopstat) =>
	      false//returns(loopstat) //TODO:check correctness
	    case IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) =>
		    elseStatement match {
		      case None => returns(ifStatement)
		      case Some(elseStat) => returns(ifStatement) && returns(elseStat) //both need to return because they are both possible
		    }
	    case ReturnStatement(_) =>
	      true //we already did typechecking!
	    case x:LocalVariableDeclaration =>
	      false
	    case WhileStatement(_, stat) =>
	      false //TODO we are not sure that we always go in the loop
	  }
	  if (m.returnType != VoidType) { //void types methods don't need a return statement
	    if (!m.implementation.isDefined) {
	      throw new ReturnException("Non void function doesn't have a body")
	    } else {
	      if (!returns(m.implementation.get))
	        throw new ReturnException("Non void function doesn't have a body")
	      else
	        returns(m.implementation.get)
	    }
	  }
	}
}