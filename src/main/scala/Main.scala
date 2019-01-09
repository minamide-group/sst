import constraint.regular.RegCons
import constraint.{Builder, relational}
import constraint.relational._
import constraint.vars._
import deterministic.{DFA, Transducer}
import deterministic.boundedcopy.SST
import scalaz.Monoid


object Main extends App {

  def printSST[S, A, B, X](s : SST[S, A, B, X]): Unit ={
    println("--------start----------")
    //println(s.states)
    println(s.states.size)
    println("----------")
    //println(s.s0)
    println("----------")
    //println(s.vars)
    println(s.vars.size)
    println("----------")
    println(s.f)
    println("----------")
    //s.δ.foreach(println)
    println(s.δ.size)
    println("----------")
    //s.η.foreach(println)
    println(s.η.size)
    println("----------")
  }

  val dfa1= DFA(
    Set(FAState(0), FAState(1), FAState(2)),
    FAState(0),
    Map(
      (FAState(0),'a') -> FAState(1),
      (FAState(1),'b') -> FAState(2),
      (FAState(2),'a') -> FAState(0)
    ),
    Set(FAState(2)))

  val builder = Builder[Char](Set('a', 'b'), '#')

  implicit def strMonoid: Monoid[List[Char]] = new Monoid[List[Char]] {
    def append(f1: List[Char], f2: => List[Char]):List[Char] = f1 ::: f2
    def zero: List[Char] = List()
  }

  val trans = Transducer(Set(
    TransState(0), TransState(1)),
    TransState(0),
    Map(
      (TransState(0), 'a') -> TransState(1),
      (TransState(0), 'b') -> TransState(1),
      (TransState(1), 'a') -> TransState(0),
      (TransState(1), 'b') -> TransState(0)
    ),
    Map(
      (TransState(0), 'a') -> List('a'),
      (TransState(0), 'b') -> List('b'),
      (TransState(1), 'a') -> List(),
      (TransState(1), 'b') -> List()
    ),
    Set(TransState(0), TransState(1))
  )

  val list = List(
    Concatenation(StringVariable(2), StringVariable(1), StringVariable(0)),
    TransducerConstraint(StringVariable(3), trans, StringVariable(2)),
    Concatenation(StringVariable(4), StringVariable(1), StringVariable(2)),
    //Concatenation(StringVariable(5), StringVariable(1), StringVariable(2)),
    //Concatenation(StringVariable(6), StringVariable(1), StringVariable(2)),
    //Concatenation(StringVariable(7), StringVariable(1), StringVariable(2)),
    //Concatenation(StringVariable(8), StringVariable(1), StringVariable(2))
  )

  val set = Set[RegCons[FAState, Char]](
    //RegCons(StringVariable(0), dfa1)
  )

  val sst = builder.constraintsToSST(list, set)

  printSST(sst)

  println(sst.process("ab#bb#"))
}