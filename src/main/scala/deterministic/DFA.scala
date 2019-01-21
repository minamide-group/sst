package deterministic

import constraint.vars.FAState
import nondeterministic.NFA


case class DFA[Q, Σ](
                      states: Set[Q],
                      s0: Q,
                      δ: Map[(Q, Σ), Q],
                      f: Set[Q]) {


  def process(input: Seq[Σ]): Boolean = toNFA.process(input)

  def toNFA: NFA[Q, Σ] = NFA(states, s0, δ.map(x => (x._1 -> (Set(x._2)))), f)

  def print {
    println("---- start print dfa ----")
    println("states: ")
    states.foreach(println)
    println("------------")
    println("s0: ")
    println(s0)
    println("------------")
    println("delta: ")
    δ.foreach(println)
    println("------------")
    println("F: ")
    f.foreach(println)
    println("------end print dfa------")
  }

  def trim: DFA[Q, Σ] = {

    def star(s: Set[Q], rules: Map[Q, Set[Q]]): Set[Q] = {
      val newS = s.flatMap(q => rules.withDefaultValue(Set())(q))
      if (newS ++ s == s) s
      else star(newS ++ s, rules)
    }

    val next0 = δ.groupBy(_._1._1).map(t => t._1 -> t._2.map(_._2).toSet)

    val reachedFromS0 = star(Set(s0), next0)

    val next = next0.filter(p => reachedFromS0(p._1)).map(p => p._1 -> p._2.filter(q => reachedFromS0(q))).filterNot(_._2.isEmpty)

    val newF = f.intersect(reachedFromS0)

    val reachToFinal = reachedFromS0.filterNot(q => star(Set(q), next).intersect(newF).isEmpty)

    DFA(
      states.intersect(reachToFinal),
      s0,
      δ.filter(r => reachToFinal(r._1._1) && reachToFinal(r._2)),
      f.filter(q => reachToFinal(q))
    )
  }

  def rename: DFA[FAState, Σ] = {
    val toNewStates = states.toList.zipWithIndex.map(t => t._1 -> FAState(t._2)).toMap

    DFA(
      states.map(q => toNewStates(q)),
      toNewStates(s0),
      δ.map(r => (toNewStates(r._1._1), r._1._2) -> toNewStates(r._2)),
      f.map(q => toNewStates(q))
    )
  }

  def minimize: DFA[Set[Q], Σ] = {

    val charSet = δ.map(r => r._1._2).toSet

    def getKey(q: Q, p: Set[Set[Q]]): Map[Σ, Set[Q]] = {
      val toP = p.flatMap(states => states.map(q => q -> states)).toMap
      charSet.collect {
        case c if δ.contains(q, c) => c -> toP(δ(q, c))
      }.toMap
    }

    def star(p0: Set[Set[Q]]): Set[Set[Q]] = {
      val p = p0.flatMap(states => states.groupBy(q => getKey(q, p0)).map(t => t._2))
      if (p == p0) p
      else star(p)
    }

    val newStates = star(Set(f, states -- f).filterNot(_.isEmpty))

    val toNewStates = newStates.flatMap(states => states.map(q => q -> states)).toMap

    DFA(
      newStates,
      toNewStates(s0),
      δ.map(r => (toNewStates(r._1._1), r._1._2) -> toNewStates(r._2)),
      f.map(q => toNewStates(q))
    )
  }

}
