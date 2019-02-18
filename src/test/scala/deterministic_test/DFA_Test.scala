package deterministic_test

import constraint.vars.FAState
import deterministic.DFA
import org.scalatest.FlatSpec

class DFA_Test extends FlatSpec{

  "minimize" should "run" in{
    val dfa = DFA(
      Set(FAState(0), FAState(1)),
      FAState(0),
      Map(
        (FAState(0), 'a') -> FAState(1),
        (FAState(1), 'a') -> FAState(1)
      ),
      Set(FAState(0), FAState(1))
    )

    val dfa1 = dfa.minimize

    assert(dfa1.states.size < dfa.states.size)
    assert(dfa.process("abca") == dfa1.process("abca"))
  }

  "intersect" should "run" in{
    val dfa0 = DFA(
      Set(FAState(0), FAState(1)),
      FAState(0),
      Map(
        (FAState(0), 'a') -> FAState(1),
        (FAState(1), 'a') -> FAState(0)
      ),
      Set(FAState(0))
    )

    val dfa1 = DFA(
      Set(FAState(0), FAState(1), FAState(2)),
      FAState(0),
      Map(
        (FAState(0), 'a') -> FAState(1),
        (FAState(1), 'a') -> FAState(2),
        (FAState(2), 'a') -> FAState(0)
      ),
      Set(FAState(0))
    )

    val dfa = dfa0.intersect(dfa1).trim.minimize.rename

    dfa.print
  }

}
