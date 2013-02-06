package parser

object Action extends Enumeration {
	type Action = Value
	val Shift, Reduce = Value
}
import Action._

trait Dfa {
	def next(symbol: Symbol): (Dfa, Action)
}