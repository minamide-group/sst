package nondeterministic

import scalaz.Monoid
import scalaz.syntax.monoid._

case class Transducer[Q, Σ, Γ](
                           states: Set[Q],
                           initStates : Set[Q],
                           δ: Set[(Q, Σ, Q, Γ)],
                           F : Set[Q]
                              )
                         (implicit val e: Monoid[Γ]) {


  def trans(input: List[Σ])(q: Q): Set[(Q, Γ)] = {
    def _trans(input: List[Σ])(cur: Set[(Q, Γ)]): Set[(Q, Γ)] = {
      input match {
        case c :: cs => _trans(cs)(cur.flatMap(x=>{δ.filter(r=>r._1==x._1).filter(r=>r._2==c).map(r=>(r._3, x._2 |+| r._4))}))
        case Nil => cur
      }
    }

    _trans(input)(Set((q, mzero)))
  }
}

