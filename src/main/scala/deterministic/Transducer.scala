package transducer

import scalaz.Monoid
import scalaz.syntax.monoid._

case class Transducer[Q, Σ, Γ](
                                states: Set[Q],
                                δ: Map[(Q, Σ), Q],
                                η: Map[(Q, Σ), Γ],
                                F: Set[Q]
                              )
                         (implicit val e: Monoid[Γ]) {

  def trans(input: List[Σ])(q: Q): (Q, Γ) = {
    def _trans(input: List[Σ])(q: Q)(m: Γ): (Q, Γ) = {
      input match {
        case c :: cs => _trans(cs)(δ(q, c))(m |+| η(q, c))
        case Nil => (q, m)
      }
    }

    _trans(input)(q)(mzero)
  }

}