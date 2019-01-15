package deterministic.factories

import constraint.vars.TransState
import deterministic.Transducer
import scalaz.Monoid

object TransducerFactory {

  implicit def strMonoid: Monoid[List[Char]] = new Monoid[List[Char]] {
    def append(f1: List[Char], f2: => List[Char]): List[Char] = f1 ::: f2

    def zero: List[Char] = List()
  }

  def getHalfTransducer() =Transducer(Set(
    TransState(0), TransState(1)),
    TransState(0),
    Map(
      (TransState(0), 'a') -> TransState(1),
      (TransState(0), 'b') -> TransState(1),
      (TransState(1), 'a') -> TransState(0),
      (TransState(1), 'b') -> TransState(0)
    ),
    Map(
      (TransState(0), 'a') -> List('a'),
      (TransState(0), 'b') -> List('b'),
      (TransState(1), 'a') -> List(),
      (TransState(1), 'b') -> List()
    ),
    Set(TransState(0), TransState(1))
  )
}
