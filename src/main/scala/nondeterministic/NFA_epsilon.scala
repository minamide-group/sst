package nondeterministic

import deterministic.DFA

case class NFA_epsilon[Q, Σ] (
                         states: Set[Q],
                         s0: Q,
                         δ: Map[(Q, Either[Σ, Boolean]), Set[Q]], // use Right(false) to express epsilon
                         f: Set[Q]
                       ){


  def toDNFA: DFA[Set[Q], Σ] = {
    type P = Set[Q]

    val r: Map[(Q, Σ), Set[Q]] = δ.filter(_._1._2.isLeft).map(t=> (t._1._1, t._1._2.left.get)->t._2 )

    def getStatesAndDelta(res1 : Set[P], res2 : Map[(P, Σ), P], queue : List[P]) : (Set[P], Map[(P, Σ), P]) ={
      queue match {
        case s :: rest => {
          val newRules: Map[(P, Σ), P] = r.filter(r => s(r._1._1)).groupBy(_._1._2).map(t =>
            (s, t._1) -> t._2.flatMap(r => r._2).toSet.flatMap(epsilonClosure(_)) )

          val newStates = newRules.map(r => r._2).filterNot(res1(_))

          getStatesAndDelta(res1 ++ newStates, res2 ++ newRules, rest ::: newStates.toList)
        }
        case Nil => (res1, res2)
      }
    }

    val initial = epsilonClosure(s0)

    val (d_states, d_rules) = getStatesAndDelta(Set(initial), Map(), List(initial))

    DFA(
      d_states,
      initial,
      d_rules,
      d_states.filter(p=>p.intersect(f).nonEmpty)
    )
  }


  def epsilonClosure(state : Q): Set[Q] ={
    if(δ.contains((state, Right(false))))
      δ((state, Right(false))) + state
    else
      Set(state)
  }
}
