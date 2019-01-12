package regex_test

import constraint.vars.FAState
import nondeterministic.NFA
import org.scalatest.FlatSpec

class NFAtoRegex_Test extends FlatSpec {

  "nfa" should "run" in {
    val q0 = FAState(0)
    val q1 = FAState(1)
    val q2 = FAState(2)

    val delta = Map(
      (q0, 'a') -> Set(q1),
      (q1, 'c') -> Set(q2),
      (q2, 'd') -> Set(q1),
      (q1, 'b') -> Set(q0)
    ).withDefaultValue(Set())

    val nfa = NFA(Set(q0, q1, q2), q0, delta, Set(q2))
    println(nfa.toRegExp)
  }
}
