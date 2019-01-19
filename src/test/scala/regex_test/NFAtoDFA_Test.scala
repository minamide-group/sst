package regex_test

import constraint.vars.FAState
import deterministic.factory.DFAFactory
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

    val factory = DFAFactory()
    val dfa1 = factory.NFAtoDFA(nfa)

    val dfa2 = factory.rename(dfa1)

    val dfa3 = factory.trim(dfa2)

    val dfa = dfa3
    dfa.states.foreach(println)
    dfa.σ.foreach(println)
    dfa.f.foreach(println)
  }
}
