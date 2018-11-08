package transducer

import scalaz.Monoid
import scalaz.syntax.monoid._

case class Transducer[Q, Σ, Γ](
                                states: Set[Q],
                                δ: Map[(Q, Σ), Q],
                                η: Map[(Q, Σ), Γ])
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


//object Test{
//  implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
//    def append(f1: Int, f2: => Int):Int = f1 + f2
//    def zero: Int = 0
//  }
//
//
//  val t = new Transducer(Set[Int](), Map[(Int,Char), Int](), Map[(Int,Char), Int]())
//}
