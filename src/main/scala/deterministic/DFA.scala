package deterministic

import nondeterministic.NFA


case class DFA[Q, Σ](
                      states: Set[Q],
                      s0: Q,
                      σ: Map[(Q,Σ), Q],
                      f: Set[Q]) {


  def process(input: Seq[Σ]): Boolean = toNFA.process(input)

  private def toNFA: NFA[Q, Σ] = NFA(states, s0, σ.map(x=>(x._1->(Set(x._2)))), f)
}
