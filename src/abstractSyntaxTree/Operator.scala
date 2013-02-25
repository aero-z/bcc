package abstractSyntaxTree

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
}