package regex_test

import constraint.vars.FAState
import deterministic.DFA
import deterministic.factory.DFAFactory
import org.scalatest.FlatSpec

class DFAtrim_Test extends FlatSpec{

  "trim" should "run" in {
    val s = List.range(0, 6).map( i => FAState(i) )

    val dfa0 = DFA(
      s.toSet,
      s(0),
      Map(
        (s(0), 'a') -> s(1),
        (s(0), 'b') -> s(2),
        (s(2), 'a') -> s(3),
        (s(4), 'a') -> s(5)
      ),
      Set(s(3))
    )

    val dfa1 = DFAFactory().trim(dfa0)

    assert(dfa1.states==Set(s(0), s(2), s(3)))
    assert(dfa1.σ== Map((s(0), 'b') -> s(2), (s(2), 'a') -> s(3)))
  }
}
