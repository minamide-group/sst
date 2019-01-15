package constraint.regular

import constraint.vars.{FAState, StringVariable}
import deterministic.DFA

case class RegCons[Σ](x : StringVariable, R : DFA[FAState, Σ])
