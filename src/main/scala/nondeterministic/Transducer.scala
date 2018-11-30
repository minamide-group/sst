package nondeterministic

import scalaz.Monoid
import scalaz.syntax.monoid._

case class Transducer[Q, Σ, Γ](
                           states: Set[Q],
                           δ: Map[(Q, Σ), Set[(Q, Γ)]],
                           F : Set[Q]
                              )
                         (implicit val e: Monoid[Γ]) {


  def trans(input: List[Σ])(q: Q): Set[(Q, Γ)] = {
    def _trans(input: List[Σ])(cur: Set[(Q, Γ)]): Set[(Q, Γ)] = {
      input match {
        case c :: cs => _trans(cs)(cur.flatMap(x=> δ(x._1, c).map(y=>(y._1, x._2 |+| y._2))))
        case Nil => cur
      }
    }

    _trans(input)(Set((q, mzero)))
  }
}

