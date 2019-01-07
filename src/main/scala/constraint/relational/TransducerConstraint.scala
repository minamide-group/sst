package constraint.relational

import constraint.vars.StringVariable

case class TransducerConstraint[Q, Σ](left : StringVariable, right1 : deterministic.Transducer[Q, Σ, List[Σ]], right2 : StringVariable) extends RelCons{
  override def getLeftIdx(): Int = left.id
}
