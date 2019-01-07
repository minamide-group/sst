package constraint.regular

import constraint.vars.StringVariable

case class RegCons[Q, Σ](x : StringVariable, R : nondeterministic.NFA[Q, Σ])
