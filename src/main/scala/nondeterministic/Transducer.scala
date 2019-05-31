package nondeterministic

import constraint.vars.TransState
import scalaz.Monoid
import scalaz.syntax.monoid._

case class Transducer[Q, Σ, Γ](
                                states: Set[Q],
                                s0: Set[Q],
                                δ: Set[(Q, Σ, Q, Γ)],
                                f: Set[Q]
                              )
                              (implicit val e: Monoid[Γ]) {


  def trans(input: List[Σ])(q: Q): Set[(Q, Γ)] = {
    def _trans(input: List[Σ])(cur: Set[(Q, Γ)]): Set[(Q, Γ)] = {
      input match {
        case c :: cs => _trans(cs)(cur.flatMap(x => {
          δ.filter(r => r._1 == x._1).filter(r => r._2 == c).map(r => (r._3, x._2 |+| r._4))
        }))
        case Nil => cur
      }
    }

    _trans(input)(Set((q, mzero)))
  }

  def print {
    println("---- start print transducer ----")
    println("states: ")
    states.foreach(println)
    println("------------")
    println("s0: ")
    s0.foreach(println)
    println("------------")
    println("delta: ")
    δ.foreach(println)
    println("------------")
    println("F: ")
    f.foreach(println)
    println("------end print transducer------")
  }

  def rename: Transducer[TransState, Σ, Γ] = {

    val toNewStates = states.toList.zipWithIndex.map(t => t._1 -> TransState(t._2)).toMap

    Transducer(
      states.map(s => toNewStates(s)),
      s0.map(s => toNewStates(s)),
      δ.map(t => (toNewStates(t._1), t._2, toNewStates(t._3), t._4)),
      f.map(s => toNewStates(s))
    )
  }

}