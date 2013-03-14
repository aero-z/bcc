package ast

//If the operations are the same for all types maybe we don't need to differ them
trait ArithmeticOperator
trait CompareOperator
trait BooleanOperator

object Operator {
    def fromString(str: String): Operator = str match {
        case "+" => PlusOperator
        case "-" => MinusOperator
        case "^"=> BitXorOperator
        case "&"=> BitAndOperator
        case "|"=> BitOrOperator
        case "!"=> InverseOperator
        case "*"=> StarOperator 
        case "/"=> DivOperator
        case "%"=> ModOperator
        case "<"=> SmallerOperator
        case ">"=> GreaterOperator
        case "=="=> EqualOperator
        case "!="=> NotEqualOperator
        case "&&"=> AndOperator
        case "||"=> OrOperator
        case "<="=> LessEqualOperator
        case ">="=> GreaterEqualOperator
    }
}

abstract class Operator(s:String) {
  override def toString = s
}
case object PlusOperator extends Operator("+") with ArithmeticOperator
case object MinusOperator extends Operator("-") with ArithmeticOperator
case object BitXorOperator extends Operator("^") with BooleanOperator
case object BitAndOperator extends Operator("&") with BooleanOperator
case object BitOrOperator extends Operator("|") with BooleanOperator
case object InverseOperator extends Operator("!") //unary -> special case!
case object StarOperator extends Operator("*") with ArithmeticOperator
case object DivOperator extends Operator("/") with ArithmeticOperator
case object ModOperator extends Operator("%") with ArithmeticOperator
case object SmallerOperator extends Operator("<") with CompareOperator
case object GreaterOperator extends Operator(">") with CompareOperator
case object EqualOperator extends Operator("==") with CompareOperator
case object NotEqualOperator extends Operator("!=") with CompareOperator
case object AndOperator extends Operator("&&") with BooleanOperator //CompareOperator?
case object OrOperator extends Operator("||") with BooleanOperator //CompareOperator?
case object LessEqualOperator extends Operator("<=") with CompareOperator
case object GreaterEqualOperator extends Operator(">=") with CompareOperator

/*
object Operator extends Enumeration {
    type Operator = Value
    val plus, minus, bitXor, bitAnd, bitOr,
    comp, star, div, mod, less, more, eq, 
    neq, condAnd, condOr, leq, geq = Value

    def fromString(str: String): Operator = str match {
        case "+" => plus
        case  "-" => minus
        case "^"=> bitXor
        case "&"=> bitAnd
        case "|"=> bitOr
        case "!"=> comp
        case "*"=> star
        case "/"=> div
        case "%"=> mod
        case "<"=> less
        case ">"=> more
        case "=="=> eq
        case "!="=> neq
        case "&&"=> condAnd
        case "||"=> condOr
        case "<="=> leq
        case ">="=> geq
    }
}*/
