package nondeterministic

import constraint.vars.FAState
import deterministic.DFA
import expression.regex._


case class NFA[Q, Σ](
                      states: Set[Q],
                      s0: Q,
                      δ: Map[(Q, Σ), Set[Q]],
                      f: Set[Q]) {

  def process(input: Seq[Σ]): Boolean = {
    val finalStates = trans(input)(Set(s0)).intersect(f)
    !finalStates.isEmpty
  }

  def trans(input: Seq[Σ])(cur: Set[Q]): Set[Q] = {
    input match {
      case Seq(c, cs@_*) => trans(cs)(cur.flatMap(x => δ.withDefaultValue(Set())(x, c)))
      case _ => cur
    }
  }

  def toRegExp: RegExp = {
    def getCombined[Q](q1: Q, q2: Q, rules: Set[(Q, RegExp, Q)]): RegExp =
      rules.filter(x => x._1 == q1).filter(x => x._3 == q2).map(x => x._2).foldLeft(EmptyExp: RegExp) { (x, y) => AltExp(x, y) }

    def eliminate(states: List[Q], rules: Set[(Q, RegExp, Q)]): Set[(Q, RegExp, Q)] = {
      states match {
        case s :: rest =>
          val newRules: Set[(Q, RegExp, Q)] = rules.filter(x => x._3 == s).flatMap(r1 => {
            rules.filter(x => x._1 == s).map(r2 => (r1._1, AltExp(getCombined(r1._1, r2._3, rules), ConcatExp(ConcatExp(r1._2, StarExp(getCombined(s, s, rules))), r2._2)), r2._3))
          })
          eliminate(rest, newRules.union(rules.filterNot(x => x._1 == s).filterNot(x => x._3 == s).filterNot(x => newRules.map(r => (r._1, r._3)).contains(x._1, x._3))))
        case Nil => rules
      }
    }

    f.map(q_f => {
      val rules: Set[(Q, RegExp, Q)] = eliminate(states.filterNot(x => x == s0).filterNot(x => x == q_f).toList, δ.toSet.flatMap((r: ((Q, Σ), Set[Q])) => r._2.map(q1 => (r._1._1, CharExp(r._1._2), q1))))

      s0 match {
        case q if q == q_f =>
          rules.filter(x => x._1 == s0).filter(x => x._3 == s0).map(x => x._2).foldLeft(EpsExp: RegExp) { (x, y) => AltExp(x, y) }
        case _ =>
          val A = getCombined(s0, s0, rules)
          val B = getCombined(s0, q_f, rules)
          val C = getCombined(q_f, s0, rules)
          val D = getCombined(q_f, q_f, rules)
          ConcatExp(ConcatExp(StarExp(AltExp(A, ConcatExp(ConcatExp(B, StarExp(D)), C))), B), StarExp(D))
      }
    }).foldLeft(EmptyExp: RegExp) { (x, y) => AltExp(x, y) }
  }

  def print {
    println("---- start print nfa ----")
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

  def rename: NFA[FAState, Σ] = {
    val toNewStates = states.toList.zipWithIndex.map(t => t._1 -> FAState(t._2)).toMap

    NFA(
      states.map(q => toNewStates(q)),
      toNewStates(s0),
      δ.map(r => (toNewStates(r._1._1), r._1._2) -> r._2.map(s => toNewStates(s))),
      f.map(q => toNewStates(q))
    )
  }

  def toDFA: DFA[Set[Q], Σ] = {
    type P = Set[Q]
    def getStatesAndDelta(res1: Set[P],
                          res2: Map[(P, Σ), P],
                          queue: List[P]): (Set[P], Map[(P, Σ), P]) = {
      queue match {
        case s :: rest => {
          val newRules: Map[(P, Σ), P] = δ.filter(r => s(r._1._1)).groupBy(_._1._2).map(t =>
            (s, t._1) -> t._2.flatMap(r => r._2).toSet )

          val newStates = newRules.map(r => r._2).filterNot(q => res1(q))

          getStatesAndDelta(res1 ++ newStates, res2 ++ newRules, rest ::: newStates.toList)
        }
        case Nil => (res1, res2)
      }
    }

    val (states, delta) = getStatesAndDelta(Set(Set(s0)), Map(), List(Set(s0)))

    DFA(
      states,
      Set(s0),
      delta,
      states.filter(s => s.intersect(f).nonEmpty)
    )
  }

}