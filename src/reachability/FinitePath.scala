package reachability

import ast._
import main.Logger.debug
import main.Joosc

class ReturnException(message: String) extends main.CompilerError(message, "Return check ")
class CatchableReturnException(message: String) extends main.CompilerError(message, "Return check ")

object FinitePath {

  def check(cus: List[CompilationUnit]) = {

    def resolveNumber(expr: Expression): Int = {
      debug("---resolveNumber: " + expr); expr match {
        case ParenthesizedExpression(expr) =>
          resolveNumber(expr)
        case BinaryOperation(first: Expression, operator: ArithmeticOperator, second: Expression) =>
          operator match {
            case PlusOperator =>
              resolveNumber(first) + resolveNumber(second)
            case MinusOperator =>
              resolveNumber(first) - resolveNumber(second)
            case StarOperator =>
              resolveNumber(first) * resolveNumber(second)
            case DivOperator =>
              resolveNumber(first) / resolveNumber(second)
            case ModOperator =>
              resolveNumber(first) % resolveNumber(second)
          }
        case x: NumberLiteral =>
          x.int
        case puree =>
          throw new CatchableReturnException("cannot resolve: " + puree)
      }
    }
    def compare(expr: Expression): Boolean = {
      debug("---compare: " + expr); expr match {
        //intersection CompareOperator inter BooleanOperator: == !=
        case ParenthesizedExpression(expr) =>
          compare(expr)
        case BinaryOperation(first: BooleanLiteral, operator: CompareOperator, second: BooleanLiteral) =>
          operator match {
            case EqualOperator => first.bool == second.bool
            case NotEqualOperator => first.bool != second.bool
          }
        case BinaryOperation(first: Expression, operator: CompareOperator, second: BooleanLiteral) =>
          operator match {
            case EqualOperator => compare(first) == second.bool
            case NotEqualOperator => compare(first) != second.bool
          }
        case BinaryOperation(first: BooleanLiteral, operator: CompareOperator, second: Expression) =>
          operator match {
            case EqualOperator => first.bool == compare(second)
            case NotEqualOperator => first.bool != compare(second)
          }
        case BinaryOperation(first: NumberLiteral, operator: CompareOperator, second: NumberLiteral) =>
          operator match {
            case SmallerOperator =>
              first.int < second.int
            case GreaterOperator =>
              first.int > second.int
            case EqualOperator =>
              first.int == second.int
            case NotEqualOperator =>
              first.int != second.int
            case LessEqualOperator =>
              first.int <= second.int
            case GreaterEqualOperator =>
              first.int >= second.int
          }
        case BinaryOperation(first: Expression, operator: CompareOperator, second: NumberLiteral) =>
          operator match {
            case SmallerOperator =>
              resolveNumber(first) < second.int
            case GreaterOperator =>
              resolveNumber(first) > second.int
            case EqualOperator =>
              resolveNumber(first) == second.int
            case NotEqualOperator =>
              resolveNumber(first) != second.int
            case LessEqualOperator =>
              resolveNumber(first) <= second.int
            case GreaterEqualOperator =>
              resolveNumber(first) >= second.int
          }
        case BinaryOperation(first: NumberLiteral, operator: CompareOperator, second: Expression) =>
          operator match {
            case SmallerOperator =>
              first.int < resolveNumber(second)
            case GreaterOperator =>
              first.int > resolveNumber(second)
            case EqualOperator =>
              first.int == resolveNumber(second)
            case NotEqualOperator =>
              first.int != resolveNumber(second)
            case LessEqualOperator =>
              first.int <= resolveNumber(second)
            case GreaterEqualOperator =>
              first.int >= resolveNumber(second)
          }
        //BooleanOperator intersection Compareoperator: == and !=
        case BinaryOperation(first @ BinaryOperation(_, _: BooleanOperator, _), operator: CompareOperator, second @ BinaryOperation(_, _: BooleanOperator, _)) =>
          operator match {
            case EqualOperator => compare(first) == compare(second)
            case NotEqualOperator => compare(first) != compare(second)
          }
        //we already catched the easier versions befor!
        case BinaryOperation(first @ BinaryOperation(_, _: ArithmeticOperator, _), operator: CompareOperator, second @ BinaryOperation(_, _: ArithmeticOperator, _)) =>
          operator match {
            case SmallerOperator =>
              resolveNumber(first) < resolveNumber(second)
            case GreaterOperator =>
              resolveNumber(first) > resolveNumber(second)
            case EqualOperator =>
              resolveNumber(first) == resolveNumber(second)
            case NotEqualOperator =>
              resolveNumber(first) != resolveNumber(second)
            case LessEqualOperator =>
              resolveNumber(first) <= resolveNumber(second)
            case GreaterEqualOperator =>
              resolveNumber(first) >= resolveNumber(second)
          }
        case puree =>
          throw new CatchableReturnException("cannot resolve: " + puree)
      }
    }
    def resolveBoolean(expr: Expression): Boolean = {
      debug("---resolveBoolean: " + expr); expr match {
        case UnaryOperation(InverseOperator, expr: Expression) =>
          !resolveBoolean(expr)
        case ParenthesizedExpression(expr) =>
          resolveBoolean(expr)
        case BinaryOperation(first: Expression, operator: BooleanOperator, second: Expression) =>
          operator match {
            case _: CompareOperator =>
              try {
                compare(expr)
              } catch {
                case e: Exception => throw new CatchableReturnException("probably not a constant expression...")
              }
            case BitXorOperator =>
              resolveBoolean(first) ^ resolveBoolean(second)
            case BitAndOperator =>
              resolveBoolean(first) & resolveBoolean(second)
            case BitOrOperator =>
              resolveBoolean(first) | resolveBoolean(second)
            case AndOperator =>
              resolveBoolean(first) && resolveBoolean(second)
            case OrOperator =>
              resolveBoolean(first) || resolveBoolean(second)
          }
        case BooleanLiteral(bool) =>
          debug("bool: " + bool)
          bool
        case puree =>
          throw new CatchableReturnException("cannot resolve: " + puree)
      }
    }
    def checkMethod(m: MethodDeclaration) {
      def onlyEmptyStatements(list: List[Statement]): Boolean = { //should only be 
        val empty = list.filter(_ != EmptyStatement).isEmpty
        if (!empty)
          throw new ReturnException("Non empty list of statements!")
        empty //true
      }
      def returns(list: List[Statement]): Boolean = list match { //we use Booleans for a lazy evaluation
        case Nil =>
          debug("Empty list")
          false
        case x :: Nil =>
          x match {
            case Block(statements) =>
              debug("Block")
              returns(statements.filter(_ != EmptyStatement)) //filter once before already
            case EmptyStatement =>
              debug("Empty")
              false
            case ExpressionStatement(_) =>
              debug("Expression")
              false
            case ForStatement(_, cond, _, body) =>
              debug("for")
              try {
                val c =
                  if (cond.isDefined)
                    resolveBoolean(cond.get)
                  else
                    true //no condition = true
                if (c) {
                  returns(body :: Nil)
                } else //unreachable body
                  throw new ReturnException("for body unreachable") //unreachable body
              } catch {
                case e: CatchableReturnException =>
                  debug("condition resolve error for for: " + e)
                  returns(body :: Nil) && false //no lines after
              }
            case IfStatement(cond, ifpart, elseOpt) =>
              debug("if stat")
              elseOpt match {
                case None =>
                  try {
                    val c = resolveBoolean(cond)
                    if (c) { //always true
                      debug("ifpart")
                      returns(ifpart :: Nil) //&& false //TODO, correct? if cannot be the last statement
                    } else
                      throw new ReturnException("if body unreachable")
                  } catch {
                    case e: CatchableReturnException =>
                      debug("condition resolve error for if: " + e)
                      returns(ifpart :: Nil) && false //no statement left
                  }
                case Some(elsepart) =>
                  try {
                    val c = resolveBoolean(cond)
                    if (c) { //always true
                      debug("ifpart")
                      val discard = returns(elsepart :: Nil)
                      returns(ifpart :: Nil)
                    } else {
                      debug("elsepart")
                      val discard = returns(ifpart :: Nil)
                      returns(elsepart :: Nil)
                    }
                  } catch {
                    case e: CatchableReturnException =>
                      debug("condition resolve error for ifelse: " + e)
                      returns(ifpart :: Nil) && returns(elsepart :: Nil) //they both have to return!
                  }
              }
            case ReturnStatement(_) =>
              debug("return")
              true //we already did typechecking!
            case LocalVariableDeclaration(typeName, identifier, _) =>
              debug("variable declaration:" + typeName + " " + identifier)
              false
            case WhileStatement(cond, stat) =>
              debug("while")
              try {
                val c = resolveBoolean(cond)
                if (c)
                  returns(stat :: Nil) || true
                else
                  throw new ReturnException("while body unreachable")
              } catch {
                case e: CatchableReturnException =>
                  debug("condition resolve error for while: " + e)
                  returns(stat :: Nil) && false //no lines after
              }
          }
        case x :: xs =>
          x match {
            case Block(statements) =>
              debug("Block xs")
              if (returns(statements.filter(_ != EmptyStatement)))
                onlyEmptyStatements(xs)
              else
                returns(xs)
            case EmptyStatement =>
              debug("Empty xs")
              returns(xs)
            case ExpressionStatement(_) =>
              debug("Expression xs")
              returns(xs)
            case ForStatement(_, cond, _, body) =>
              debug("For xs")
              try {
                val c =
                  if (cond.isDefined)
                    resolveBoolean(cond.get)
                  else
                    true
                debug("boolean c: " + c)
                if (c)
                  returns(body :: Nil) && onlyEmptyStatements(xs)
                else
                  throw new ReturnException("for body unreachable")
              } catch {
                case e: CatchableReturnException =>
                  (returns(body :: Nil) && false) || returns(xs)
              }
            case IfStatement(cond, ifpart, elseOpt) =>
              debug("If xs")
              elseOpt match {
                case None =>
                  try {
                    val c = resolveBoolean(cond)
                    if (c)
                      (returns(ifpart :: Nil) && false) || returns(xs)
                    else
                      returns(xs)
                  } catch {
                    case e: CatchableReturnException =>
                      (returns(ifpart :: Nil) && false) || returns(xs)
                  }
                case Some(elsepart) =>
                  try {
                    val c = resolveBoolean(cond)
                    if (c) {
                      debug("ifpart:")
                      (returns(elsepart :: Nil) || true) && returns(ifpart :: Nil) && onlyEmptyStatements(xs)
                    } else {
                      debug("elsepart:")
                      (returns(ifpart :: Nil) || true) && returns(elsepart :: Nil) && onlyEmptyStatements(xs)
                    }
                  } catch {
                    case e: CatchableReturnException =>
                      if (returns(ifpart :: Nil) && returns(elsepart :: Nil))
                        onlyEmptyStatements(xs)
                      else
                        returns(xs)
                  }

              }
            case ReturnStatement(_) =>
              debug("Return xs")
              val ret = onlyEmptyStatements(xs)
              if (!ret) { //reachability check
                throw new ReturnException("non empty statements after return!")
              }
              false //not used
            case LocalVariableDeclaration(typeName, identifier, _) =>
              debug("variable declaration:" + typeName + " " + identifier)
              returns(xs)
            case WhileStatement(cond, stat) =>
              debug("while xs")
              try {
                val c = resolveBoolean(cond)
                if (c)
                  (returns(stat :: Nil) || true) && onlyEmptyStatements(xs) //does not need to return because it is infinite
                else
                  throw new ReturnException("while body unreachable")
              } catch {
                case e: CatchableReturnException =>
                  (returns(stat :: Nil) && true) || returns(xs)
              }
          }
      }
      debug(m.methodName + ":")
      if (m.returnType != VoidType) { //void types methods don't need a return statement
        if (!m.implementation.isDefined) {
          throw new ReturnException("Non void function doesn't have a body")
        } else {
          if (!returns(m.implementation.get :: Nil))
            throw new ReturnException("unreachable code due to return or does not return...")
        }
      } else { //void functions
        if (m.implementation.isDefined) {
          val discard = returns(m.implementation.get :: Nil)
        }
      }
    }

    cus.flatMap {
      case CompilationUnit(_, _, Some(c: ClassDefinition), name) =>
        debug(">>>>>>>>>> " + name);
        c.methods
      case _ => Nil
    }.filter(
      x => !x.modifiers.contains(Modifier.abstractModifier) && !x.modifiers.contains(Modifier.nativeModifier)).foreach(checkMethod(_))
    // for constructors
    cus.flatMap {
      case CompilationUnit(_, _, Some(c: ClassDefinition), name) =>
        debug(">>>>>>>>>> " + name);
        c.constructors
      case _ => Nil
    }.filter(
      x => !x.modifiers.contains(Modifier.abstractModifier) && !x.modifiers.contains(Modifier.nativeModifier)).foreach(
        x => checkMethod(MethodDeclaration(x.name, VoidType, x.modifiers, x.parameters, Some(x.implementation),x.localPath)))
  }
}
