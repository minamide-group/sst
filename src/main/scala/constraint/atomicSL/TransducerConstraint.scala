package constraint.atomicSL

import constraint.vars.{StringVariable, TransState}

case class TransducerConstraint[Σ](left : Int, fst : deterministic.Transducer[TransState, Σ, List[Σ]], source : Int) extends AtomicSLCons{
  override def getLeftIdx(): Int = left
}
