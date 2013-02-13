package parser

import scala.io.Source
import scanner.Token

abstract class Action
  
case class ShiftAction(state: Dfa.State) extends Action
case class ReduceAction(rule: Rule) extends Action
case class ErrorAction() extends Action

trait Dfa {
    
  def delta(q: Dfa.State, i: Symbol): Action
  val q0 = 0
}

package object Dfa {
  type State = Int

  def fromFile(file: Source): Dfa = {
    
    val syntaxErrException = new RuntimeException("syntax error in LR1 file")
    
    //def extractTerminals(lines:List[String]) = lines.map(_ => ???)
    //def extractNonTerminals(lines:List[String]) = lines.map(_ => ???)

    val lines = file.getLines.toList
    val numt = Integer.parseInt(lines.head)
    //val terminals = extractTerminals(lines.tail.take(numt))

    val lines2 = lines.drop(numt+1)
    val numnt = Integer.parseInt(lines2.head)
    //val nonterminals = extractNonTerminals(lines2.tail.take(numnt))

    val lines3 = lines2.drop(numnt+2)
    val numrules = Integer.parseInt(lines3.head)
    
    // TODO: read in rules or get rules from some other place
    
    val rulesRaw = lines3.drop(1).take(numrules).map(
        _.split("\\s+")
    )
    val rules = rulesRaw.map(l => Rule(l.head, l.size-1)).to[Vector]

    val lines4 = lines3.drop(numrules+1)
    val numstates = Integer.parseInt(lines4.head)
    val numtrans = Integer.parseInt(lines4.tail.head)
        
    val state = ("""(\d+) (\S+) (\S+) (\d+)""").r;
    val t = lines4.drop(2).map(_ match {
      case state(state, symbol, action, nextState) => (state.toInt, symbol, action, nextState.toInt);
      case _ => throw syntaxErrException
    })    
    
    // NOTE: possible optimization: make a map (State, Symbol) => Action
    
    def symbolRepr(s: Symbol) = {
      s match {
        //TODO
        case NonTerminalSymbol(str, _) => str
        case x => {
          if (x.isInstanceOf[Token]) {
            x.asInstanceOf[Token].typeStr
          }
          else throw syntaxErrException
        }
      }
    }
    
    new Dfa {
      def delta(q: State, i: Symbol) = {
        t.find(x => x._1 == q && x._2 == symbolRepr(i)) match {
          case Some((_, _, actionStr, int)) => {
            actionStr match {
	            case "reduce" => ReduceAction(rules(int))
	            case "shift" => ShiftAction(int)
	            case _ => throw syntaxErrException
            }
          }
          case None => ErrorAction()
        }
      }
    }
  }
}

