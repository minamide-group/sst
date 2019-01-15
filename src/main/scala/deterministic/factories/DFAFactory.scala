package deterministic.factories

import constraint.vars.FAState
import deterministic.DFA

object DFAFactory {

  def getDFA0 = DFA(
    Set(FAState(0), FAState(1), FAState(2)),
    FAState(0),
    Map(
      (FAState(0), 'a') -> FAState(1),
      (FAState(1), 'b') -> FAState(2),
      (FAState(2), 'a') -> FAState(0)
    ),
    Set(FAState(2)))
}
