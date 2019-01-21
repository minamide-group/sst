package regex_test

import constraint.vars.FAState
import deterministic.DFA
import nondeterministic.NFA
import org.scalatest.FlatSpec

class NFAtoDFA_Test extends FlatSpec{

  "program" should "run" in{
    val nfa = NFA(
      Set(FAState(0), FAState(1), FAState(2)),
      FAState(0),
      Map(
        (FAState(0), 'a')-> Set(FAState(0), FAState(1)),
        (FAState(0), 'b')-> Set(FAState(0)),
        (FAState(1), 'b')-> Set(FAState(2))
      ),
      Set(FAState(2))
    )

    val dfa1 = nfa.toDFA

    val dfa2 = dfa1.rename

    val dfa3 = dfa2.trim

    val dfa = dfa3
    dfa.states.foreach(println)
    dfa.δ.foreach(println)
    dfa.f.foreach(println)
  }
}
