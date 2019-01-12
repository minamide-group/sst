package constraint.relational

import constraint.vars.{StringVariable, TransState}

case class TransducerConstraint[Σ](left : StringVariable, right1 : deterministic.Transducer[TransState, Σ, List[Σ]], right2 : StringVariable) extends RelCons{
  override def getLeftIdx(): Int = left.id
}
