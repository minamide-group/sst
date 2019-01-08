package constraint.regular

import constraint.vars.StringVariable
import deterministic.DFA

case class RegCons[Q, Σ](x : StringVariable, R : DFA[Q, Σ])
