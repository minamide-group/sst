package constraint.atomicSL

case class Concatenation[Σ](left :Int, list : List[Either[Int, Σ]]) extends AtomicSLCons{
  override def getLeftIdx(): Int = left
}

