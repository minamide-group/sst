package deterministic

import nondeterministic.Transducer
import scalaz.Monoid

case class SST[Q, Σ, Γ, X](//state, input alphabet, output alphabet, variable
                           states: Set[Q],
                           s0: Q,
                           vars: Set[X],
                           δ: Map[(Q, Σ), Q],
                           η: Map[(Q, Σ), Map[X, List[Either[X, Γ]]]],
                           f: Map[Q, List[Either[X, Γ]]]
                          ) {

  /**
    * @param input
    * @return (isAccepted, finialState, outputSequence)
    */
  def process(input: Seq[Σ]): (Boolean, Q, Seq[Γ]) = {
    def _process(input: Seq[Σ])(q: Q)(env: Map[X, Seq[Γ]]): (Q, Map[X, Seq[Γ]]) = {
      input match {
        case Seq(c, cs@_*) => _process(cs)(δ(q, c))(η(q, c).map(x => (x._1, eval(x._2, env))))
        case _ => (q, env)
      }
    }

    val result = _process(input)(s0)(vars.map(x => (x, Seq())).toMap)
    //isAccepted, finialState, output Sequence
    (f.contains(result._1), result._1, eval(f(result._1), result._2))
  }

  def eval(list: List[Either[X, Γ]], env: Map[X, Seq[Γ]]): Seq[Γ] = {
    def _eval(list: List[Either[X, Γ]], cur: Seq[Γ]): Seq[Γ] = {
      list match {
        case next :: rest =>
          next match {
            case Left(x) => _eval(rest, cur ++ env(x))
            case Right(gamma) => _eval(rest, cur :+ gamma)
          }
        case _ => cur
      }
    }

    _eval(list, Seq())
  }

  /**
    * @param input : input sequence
    * @param q     : start state
    * @return output function X=> (X∪Γ)*
    */
  def trans(input: Seq[Σ])(q: Q): (Q, Map[X, List[Either[X, Γ]]]) = {
    def _trans(input: Seq[Σ])(q: Q)(m: Map[X, List[Either[X, Γ]]]): (Q, Map[X, List[Either[X, Γ]]]) = {
      input match {
        case Seq(c, cs@_*) => _trans(cs)(δ(q, c))(composite(m, η(q, c)))
        case _ => (q, m)
      }
    }

    val result = _trans(input)(q)(vars.map(x => (x, List(Left(x)))).toMap) //initially m is λx.x
    //finialState, output function
    (result._1, result._2)
  }

  /**
    * @param m1 : X=> (X∪Γ)*
    * @param m2 : X=> (X∪Γ)*
    * @return m1·m2 : X=> (X∪Γ)*
    */
  def composite(m1: Map[X, List[Either[X, Γ]]], m2: Map[X, List[Either[X, Γ]]]): Map[X, List[Either[X, Γ]]] = {
    def _composite(m1: Map[X, List[Either[X, Γ]]], list: List[Either[X, Γ]], ret: List[Either[X, Γ]]): List[Either[X, Γ]] = {
      list match {
        case next :: rest =>
          next match {
            case Left(x) => _composite(m1, rest, ret ::: m1(x))
            case Right(gamma) => _composite(m1, rest, ret :+ Right(gamma))
          }
        case _ => ret
      }
    }

    vars.map(x => (x, _composite(m1, m2(x), List()))).toMap
  }

  /**
    * @return nondeterministic transducer that ouput variable update functions
    */
  def toNondeterminTransducer : Transducer[Q, Σ,  Map[X, List[Either[X, Γ]]]]={
    implicit def monoid: Monoid[Map[X, List[Either[X, Γ]]]] = new Monoid[Map[X, List[Either[X, Γ]]]] {
      def append(m1: Map[X, List[Either[X, Γ]]], m2: =>Map[X, List[Either[X, Γ]]]): Map[X, List[Either[X, Γ]]] = composite(m1,m2)
      def zero: Map[X, List[Either[X, Γ]]] = vars.map(x => (x, List(Left(x)))).toMap
    }

    Transducer(states, δ.map(x=>(x._1->(Set( (x._2, η(x._1)) )))))
  }
}
