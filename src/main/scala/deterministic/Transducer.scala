package deterministic

import constraint.vars.TransState
import scalaz.Monoid
import scalaz.syntax.monoid._

case class Transducer[Q, Σ, Γ](
                                states: Set[Q],
                                s0: Q,
                                δ: Map[(Q, Σ), Q],
                                η: Map[(Q, Σ), Γ],
                                f: Set[Q]
                              )
                              (implicit val e: Monoid[Γ]) {

  def trans(input: List[Σ])(q: Q): (Q, Γ) = {
    def _trans(input: List[Σ])(q: Q)(m: Γ): (Q, Γ) = {
      input match {
        case c :: cs => _trans(cs)(δ(q, c))(m |+| η(q, c))
        case Nil => (q, m)
      }
    }

    _trans(input)(q)(mzero)
  }

  def print {
    println("---- start print transducer ----")
    println("states: ")
    states.foreach(println)
    println("------------")
    println("s0: ")
    println(s0)
    println("------------")
    println("delta: ")
    δ.foreach(println)
    println("------------")
    println("eta: ")
    η.foreach(println)
    println("------------")
    println("F: ")
    f.foreach(println)
    println("------end print transducer------")
  }

  def rename: Transducer[TransState, Σ, Γ] = {
    val toNewStates = states.toList.zipWithIndex.map(t => t._1 -> TransState(t._2)).toMap
    Transducer(
      states.map(s => toNewStates(s)),
      toNewStates.withDefaultValue(TransState(0))(s0),
      δ.map(t => (toNewStates(t._1._1), t._1._2) -> toNewStates(t._2)),
      η.map(t => (toNewStates(t._1._1), t._1._2) -> t._2),
      f.map(s => toNewStates(s))
    )
  }

  def toDFA: DFA[Q, Σ] = DFA(states, s0, δ, f)

  def trim = {
    val dfa = toDFA.trim
    Transducer(dfa.states, dfa.s0, dfa.δ, η.filter(r=> dfa.δ.contains(r._1)), dfa.f)
  }

  def intersect[Q1](dfa : DFA[Q1,Σ]) = {
    val res0 : DFA[(Q, Q1), Σ]= toDFA.intersect(dfa)
    val newEta = res0.δ.map(r=> r._1-> η(r._1._1._1, r._1._2))
    Transducer(res0.states, res0.s0, res0.δ, newEta, res0.f)
  }
}
