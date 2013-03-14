package ast

//If the operations are the same for all types maybe we don't need to differ them
trait IntegerOperator extends ShortOperator
trait ShortOperator extends ByteOperator
trait ByteOperator
trait CharOperator extends ByteOperator
trait CompareOperator

object Operator {
    def fromString(str: String): Operator = str match {
        case "+" => PlusOperator()
        case  "-" => MinusOperator()
        case "^"=> BitXorOperator()
        case "&"=> BitAndOperator()
        case "|"=> BitOrOperator()
        case "!"=> InverseOperator()
        case "*"=> StarOperator() 
        case "/"=> DivOperator()
        case "%"=> ModOperator()
        case "<"=> SmallerOperator()
        case ">"=> GreaterOperator()
        case "=="=> EqualOperator()
        case "!="=> NotEqualOperator()
        case "&&"=> AndOperator()
        case "||"=> OrOperator()
        case "<="=> LessEqualOperator()
        case ">="=> GreaterEqualOperator()
    }
}

abstract class Operator(s:String) {
  def toString = s
}
case class PlusOperator() extends Operator("+") with ByteOperator
case class MinusOperator() extends Operator("-") with ByteOperator
case class BitXorOperator() extends Operator("")
case class BitAndOperator() extends Operator("&")
case class BitOrOperator() extends Operator("|")
case class InverseOperator() extends Operator("!")
case class StarOperator() extends Operator("*")
case class DivOperator() extends Operator("/")
case class ModOperator() extends Operator("%")
case class SmallerOperator() extends Operator("<") with ComparatorOperator
case class GreaterOperator() extends Operator(">") with ComparatorOperator
case class EqualOperator() extends Operator("==") with ComparatorOperator
case class NotEqualOperator() extends Operator("!=") with ComparatorOperator
case class AndOperator() extends Operator("&&") with ComparatorOperator
case class OrOperator() extends Operator("||") with ComparatorOperator
case class LessEqualOperator() extends Operator("<=") with ComparatorOperator
case class GreaterEqualOperator() extends Operator(">=") with ComparatorOperator

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