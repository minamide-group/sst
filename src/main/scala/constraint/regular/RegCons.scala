package constraint.regular

import constraint.vars.FAState
import deterministic.DFA

case class RegCons[Σ](x: Int, R: DFA[FAState, Σ])
