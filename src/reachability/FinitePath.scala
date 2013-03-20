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
    def resolveBoolean(expr:Expression):Boolean = expr match {
      case UnaryOperation(InverseOperator, expr:Expression) =>
        !resolveBoolean(expr)
      case BinaryOperation(first: Expression, operation:BooleanOperator, second: Expression) =>
        operation match {
          case BitXorOperator =>
            resolveBoolean(first) ^ resolveBoolean(second)
          case BitAndOperator =>
            resolveBoolean(first) & resolveBoolean(second)
          case BitOrOperator =>
            resolveBoolean(first) | resolveBoolean(second)
          case EqualOperator =>
            resolveBoolean(first) == resolveBoolean(second)
          case NotEqualOperator =>
            resolveBoolean(first) != resolveBoolean(second)
          case AndOperator =>
            resolveBoolean(first) && resolveBoolean(second)
          case OrOperator =>
            resolveBoolean(first) || resolveBoolean(second)
        }
      case BooleanLiteral(bool) =>
        bool
      case puree =>
        throw new ReturnException("cannot resolve: "+puree)
      //case FieldAccess(accessed: Expression, field: String) => 
        
      //case Assignment(leftHandSide: Expression, rightHandSide: Expression) =>
    }
	def check(m:MethodDeclaration) {
	  def onlyEmptyStatements(list:List[Statement]):Boolean = {
	    list.filter(_ != EmptyStatement).isEmpty
	  }
	  def returns(list:List[Statement]):Boolean = list match { //we use Booleans for a lazy evaluation
	    case Nil =>
	      println("Empty list")
	      false
	    case x :: Nil =>
	      x match {
	        case Block(statements) =>
	          println("Block")
			  returns(statements.filter(_ != EmptyStatement))
		    case EmptyStatement =>
		      println("Empty")
		      false
		    case ExpressionStatement(_) =>
		      println("Expression")
		      false
		    case ForStatement(_, cond, _, body) =>
		      try {
		        val c =
		        	if (cond.isDefined)
		        	  resolveBoolean(cond.get)
		        	else
		        	  true //no condition = true
				if (c) {
				  returns(body::Nil)
				} else
				  false //unreachable body
		      } catch {
		        case e:Exception =>
		          println("condition resolve error for for: "+e)
		          false //no lines after
		      }
		    case IfStatement(cond, ifpart, elseOpt) =>
			    elseOpt match {
			      case None =>
			        try {
				        val c = resolveBoolean(cond)
				        if (c) { //always true
				          returns(ifpart::Nil) && false //if cannot be the last statement
				        } else
				          false //no lines after
				    } catch {
				        case e:Exception =>
				        println("condition resolve error for if: "+e)
				        false //no statement left
				    }
			      case Some(elsepart) =>
			        try {
				        val c = resolveBoolean(cond)
				        if (c) { //always true
				          returns(ifpart::Nil)
				        } else
				          returns(elsepart::Nil)
				    } catch {
				        case e:Exception =>
				        println("condition resolve error for ifelse: "+e)
				        returns(ifpart::Nil) && returns(elsepart::Nil) //they both have to return!
				    }
			    }
		    case ReturnStatement(_) =>
		      println("return")
		      true //we already did typechecking!
		    case LocalVariableDeclaration(typeName, identifier, _) =>
		      println("variable declaration:"+typeName+" "+identifier)
		      false
		    case WhileStatement(cond, stat) =>
		      try {
		        val c = resolveBoolean(cond)
		        if (c)
		          returns(stat::Nil) | true
		        else
		          false //unreachable body
		      } catch {
		        case e:Exception =>
		        println("condition resolve error for while: "+e)
		        false //no lines after
		      }
	      }
	    case x :: xs =>
	      x match {
	        case Block(statements) =>
	          println("Block xs")
			  if (returns(statements.filter(_ != EmptyStatement)))
			    onlyEmptyStatements(xs)
			  else
			    returns(xs)
		    case EmptyStatement =>
		      println("Empty xs")
		      returns(xs)
		    case ExpressionStatement(_) =>
		      println("Expression xs")
		      returns(xs)
		    case ForStatement(_, cond, _, body) =>
		      try {
		        val c =
		          if (cond.isDefined)
		            resolveBoolean(cond.get)
		          else
		            true
		        if (c)
		          returns(body::Nil) && onlyEmptyStatements(xs)
		        else
		          false //unreachable body!
		      } catch {
		        case e:Exception =>
		          returns(xs)
		      }
		    case IfStatement(cond, ifpart, elseOpt) =>
		        println("If xs")
			    elseOpt match {
			      case None =>
			        try {
			          val c = resolveBoolean(cond)
			          if (c)
			            (returns(ifpart::Nil) && false) || returns(xs)
			          else
			            returns(xs)
			        } catch {
			          case e:Exception =>
			            returns(xs)
			        }
			      case Some(elsepart) =>
				      try {
				        val c = resolveBoolean(cond)
				        if (c)
				          returns(ifpart::Nil) && onlyEmptyStatements(xs)
				        else
				          returns(elsepart::Nil) && onlyEmptyStatements(xs)
				      } catch {
				        case e:Exception =>
				        returns(xs) || (returns(ifpart::Nil) && returns(elsepart::Nil) && onlyEmptyStatements(xs) )
				      }
			        
			    }
		    case ReturnStatement(_) =>
		      println("Return xs")
		      onlyEmptyStatements(xs) //reachability check
		    case LocalVariableDeclaration(typeName, identifier, _) =>
		      println("variable declaration:"+typeName+" "+identifier)
		      returns(xs)
		    case WhileStatement(cond, stat) =>
		      try {
		        val c = resolveBoolean(cond)
		        if (c)
		          (returns(stat::Nil) | true) && onlyEmptyStatements(xs) //does not need to return because it is infinite
		        else
		          false //unreachable body
		      } catch {
		        case e:Exception =>
		          returns(xs)
		      }
	      }
	  }
	  println(m.methodName+":")
	  if (m.returnType != VoidType) { //void types methods don't need a return statement
	    if (!m.implementation.isDefined) {
	      throw new ReturnException("Non void function doesn't have a body")
	    } else {
	      if (!returns(m.implementation.get::Nil))
	        throw new ReturnException("unreachable code due to return or does not return...")
	    }
	  } else { //void functions
	    if (m.implementation.isDefined) {
	      val discard = returns(m.implementation.get::Nil)
	    }
	  }
	}
}