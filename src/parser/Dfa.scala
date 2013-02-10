package parser

import scala.io.Source

abstract class Action
  
case class ShiftAction(state: Int) extends Action
case class ReduceAction(rule: Rule) extends Action

class Dfa {
    type State = Int

    
  def delta(q: State, i: Symbol): Action
  val q0 = 0
}

object Dfa {

  def fromFile(file: Source): Dfa = {
    
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

    val lines4 = lines3.drop(numrules+1)
    val numstates = Integer.parseInt(lines4.head)
    val numtrans = Integer.parseInt(lines4.tail.head)
    
    //val lines5 = lines4.drop(numtrans+2)
    
    val state = ("""(\w+) (\w+) (\w+) (\w+)""").r; // NOT working!
    val t = lines4.drop(2).map(_ match {
      case state(state, symbol, action, nextState) => (state.toInt, symbol, action, nextState.toInt);
      case _ => println _
    })
    
    println("hoi")
    t.foreach(println _)
    
    /*new Dfa {
      def delta(q: State, i: Symbol) = {
        t.find(_ match {
          case (q, i, a, n) => true
          case _ => false
        }) match {
          case Some((_, _, a, n)) => (a, n) 
          case None => ???
        }
      }
    }
    */
    ???
  }
}

