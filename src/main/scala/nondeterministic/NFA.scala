package nondeterministic

case class NFA[Q, Σ](
                  states: Set[Q],
                  s0: Q,
                  σ: Map[(Q,Σ), Set[Q]],
                  f: Set[Q]) {


  def process(input: Seq[Σ]):(Boolean, Set[Q])={
    val finalStates = trans(input)(Set(s0)).intersect(f)
    (!finalStates.isEmpty, finalStates)
  }

  def trans(input: Seq[Σ])(cur: Set[Q]): Set[Q] = {
    input match {
      case Seq(c, cs@_*) => trans(cs)(cur.flatMap(x=>σ(x,c)))
      case _ => cur
    }
  }

  def toRegEx:String = {
    ""
  }
}
