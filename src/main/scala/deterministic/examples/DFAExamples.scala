package deterministic.examples

import constraint.vars.FAState
import deterministic.DFA

object DFAExamples {

  def getDFA1 = DFA(
    Set(FAState(3)),
    FAState(3),
    Map(
      (FAState(3), 'a') -> FAState(3)
    ),
    Set(FAState(3)))

  def getDFA2 = DFA(
    Set(FAState(4), FAState(5)),
    FAState(4),
    Map(
      (FAState(4), 'a') -> FAState(4),
      (FAState(4), 'b') -> FAState(5),
      (FAState(5), 'a') -> FAState(4),
    ),
    Set(FAState(5)))

  def getDFA3 = DFA(
    Set(FAState(0), FAState(1), FAState(2)),
    FAState(0),
    Map(
      (FAState(0), 'a') -> FAState(1),
      (FAState(1), 'b') -> FAState(2),
      //(FAState(2), 'a') -> FAState(0)
    ),
    Set(FAState(2)))

  def getDFA4 = DFA(
    Set(FAState(41), FAState(42), FAState(43), FAState(44)),
    FAState(41),
    Map(
      (FAState(41), 'a') -> FAState(41),
      (FAState(41), 'b') -> FAState(42),
      (FAState(42), 'a') -> FAState(43),
      (FAState(43), 'a') -> FAState(44),
      //(FAState(44), 'a') -> FAState(41)
    ),
    Set(FAState(44)))

}
