package deterministic

import nondeterministic.NFA

case class DFA[Q, Σ](
                      states: Set[Q],
                      s0: Q,
                      σ: Map[(Q,Σ), Q],
                      f: Set[Q]) {

  def process(input: Seq[Σ]):(Boolean, Q)={
    val finalState = trans(input)(s0)
    (f(finalState), finalState)
  }

  def trans(input: Seq[Σ])(q: Q): Q = {
    input match {
      case Seq(c, cs@_*) => trans(cs)(σ(q, c))
      case _ => q
    }
  }

  def toNFA: NFA[Q, Σ] = NFA(states, s0, σ.map(x=>(x._1->(Set(x._2)))), f)

  def toRegEx = toNFA.toRegExp
}
