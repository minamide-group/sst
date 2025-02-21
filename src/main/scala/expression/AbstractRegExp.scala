package expression

import expression.regex._

trait AbstractRegExp[T, Γ] {

  def eval(r: RegExp): RegExp

  def h(a: Γ): T

  def toRegExp_d[Q](dfa : deterministic.DFA[Q, Γ]) : RegExp ={
    val (states, q0, finialStates, delta) = (dfa.states, dfa.s0, dfa.f, dfa.δ.toSet)

    finialStates.map(qf => {
      val rules = eliminate(states.filterNot(x => x == q0).filterNot(x => x == qf).toList,
        delta.map(x => (x._1._1, CharExp(h(x._1._2)): RegExp, x._2)))

      if (q0 == qf) {
        val A = getCombined(q0, q0, rules)
        StarExp(A)
      }
      else {
        val A = getCombined(q0, q0, rules)
        val B = getCombined(q0, qf, rules)
        val C = getCombined(qf, q0, rules)
        val D = getCombined(qf, qf, rules)
        ConcatExp(ConcatExp(StarExp(AltExp(A, ConcatExp(ConcatExp(B, StarExp(D)), C))), B), StarExp(D))
      }
    }).foldLeft(EmptyExp: RegExp) { (x, y) => AltExp(x, y) }
  }

  def toRegExp[Q, Σ](transducer: nondeterministic.Transducer[Q, Σ, Γ]): RegExp = {
    val (states, initialStates, finialStates, delta) = (transducer.states, transducer.s0, transducer.f, transducer.δ)

    initialStates.flatMap(q0 => {
      finialStates.map(qf => {
        val rules = eliminate(states.filterNot(x => x == q0).filterNot(x => x == qf).toList,
          delta.map(x => (x._1, CharExp(h(x._4)): RegExp, x._3)))

        if (q0 == qf) {
          val A = getCombined(q0, q0, rules)
          StarExp(A)
        }
        else {
          val A = getCombined(q0, q0, rules)
          val B = getCombined(q0, qf, rules)
          val C = getCombined(qf, q0, rules)
          val D = getCombined(qf, qf, rules)
          ConcatExp(ConcatExp(StarExp(AltExp(A, ConcatExp(ConcatExp(B, StarExp(D)), C))), B), StarExp(D))
        }
      })
    }).foldLeft(EmptyExp: RegExp) { (x, y) => AltExp(x, y) }
  }

  private def getCombined[Q](q1: Q, q2: Q, rules: Set[(Q, RegExp, Q)]): RegExp =
    rules.filter(x => x._1 == q1).filter(x => x._3 == q2).map(x => x._2).foldLeft(EmptyExp: RegExp) { (x, y) => AltExp(x, y) }

  private def eliminate[Q](states: List[Q], rules: Set[(Q, RegExp, Q)]): Set[(Q, RegExp, Q)] = {
    states match {
      case s :: rest =>
        val newRules: Set[(Q, RegExp, Q)] = rules.filterNot(x => x._1 == s && x._3 == s).filter(x => x._3 == s).flatMap(r1 => {
          rules.filterNot(x => x._1 == s && x._3 == s).filter(x => x._1 == s).map(r2 => (r1._1, AltExp(getCombined(r1._1, r2._3, rules), ConcatExp(ConcatExp(r1._2, StarExp(getCombined(s, s, rules))), r2._2)), r2._3))
        })
        eliminate(rest, newRules.union(rules.filterNot(x => x._1 == s).filterNot(x => x._3 == s).filterNot(x => newRules.map(r => (r._1, r._3)).contains(x._1, x._3))))
      case Nil => rules
    }
  }
}
