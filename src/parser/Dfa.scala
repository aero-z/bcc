package parser

/*object Action extends Enumeration {
	type Action = Value
	val Shift, Reduce = Value
}
import Action._*/

abstract class Action
case class ShiftAction extends Action
case class ReduceAction(rule: Int) extends Action

trait Dfa {
	def next(symbol: Symbol): (Dfa, Action)
}