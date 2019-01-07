import constraint.Builder
import constraint.relational._
import constraint.vars._
import nondeterministic.NFA


object Main extends App {

  val nfa1= NFA(
    Set(NFAState(0), NFAState(1), NFAState(2)),
    NFAState(0),
    Map(
      (NFAState(0),'a') -> Set(NFAState(1)),
      (NFAState(1),'b') -> Set(NFAState(2)),
      (NFAState(2),'a') -> Set(NFAState(0))
    ),
    Set(NFAState(2)))
  val builder = Builder[Int, Char](Set('a', 'b'), '#')

  val map : Map[StringVariable, NFA[NFAState, Char]] = Map(
    StringVariable(1)-> nfa1
  )
  val sst = builder.regularToSST(3, map)



  println(sst.states)
  println("-------")
  println(sst.s0)
  println("-------")
  println(sst.vars)
  println("-------")
  println(sst.f)
  println("-------")
  sst.δ.foreach(println)
  println("-------")
  sst.η.foreach(println)
  println("-------")
}