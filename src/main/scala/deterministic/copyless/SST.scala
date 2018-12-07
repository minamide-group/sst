package deterministic.copyless

import regex.{IntRegExp, MapRegExp}
import scalaz.Monoid

case class SST[Q, Σ, Γ, X](//state, input alphabet, output alphabet, variable
                           states: Set[Q],
                           s0: Q,
                           vars: Set[X],
                           δ: Map[(Q, Σ), Q],
                           η: Map[(Q, Σ), Map[X, List[Either[X, Γ]]]],
                           f: Map[Q, List[Either[X, Γ]]]
                          ) {

  def process(input: Seq[Σ]): (Boolean, Q, Seq[Γ]) = {

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

    def _process(input: Seq[Σ])(q: Q)(env: Map[X, Seq[Γ]]): (Q, Map[X, Seq[Γ]]) = {
      input match {
        case Seq(c, cs@_*) => _process(cs)(δ(q, c))(η(q, c).map(x => (x._1, eval(x._2, env))))
        case _ => (q, env)
      }
    }

    val result = _process(input)(s0)(vars.map(x => (x, Seq())).toMap)
    (f.contains(result._1), result._1, eval(f(result._1), result._2))
  }

  def trans(input: Seq[Σ])(q: Q): (Q, Map[X, List[Either[X, Γ]]]) = {

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

    def _trans(input: Seq[Σ])(q: Q)(m: Map[X, List[Either[X, Γ]]]): (Q, Map[X, List[Either[X, Γ]]]) = {
      input match {
        case Seq(c, cs@_*) => _trans(cs)(δ(q, c))(composite(m, η(q, c)))
        case _ => (q, m)
      }
    }

    _trans(input)(q)(vars.map(x => (x, List(Left(x)))).toMap) //initially m is λx.x
  }

  def toSemiLinearSet: Set[((Int, Int), Set[(Int, Int)])] = {
    val r = IntRegExp.eval(IntRegExp.toRegExp(toIntegerTransducer)) match {
      case x: IntRegExp.CharExp => x.c
      case _ => Set[((Int, Int), Set[(Int, Int)])]()
    }

    if (f.contains(s0)) r ++ Set[((Int, Int), Set[(Int, Int)])](((0, f(s0).filter(x => x.isRight).size), Set.empty)) else r
  }

  def toIntegerTransducer: nondeterministic.Transducer[Either[(Q, Set[X]), Int], Σ, Int] = {

    def h(alpha: Map[X, List[Either[X, Γ]]]): (Map[X, Set[X]], Map[X, Int]) =
      (alpha.mapValues(_.filter(x => x.isLeft).map(x => x.left.get).toSet), alpha.mapValues(_.filter(x => x.isRight).size))

    val q_bottom: Int = 0

    val delta2: Set[((Q, Set[X]), Σ, Int, Int)] = δ.map(x => (x._1._1, x._1._2, x._2, h(η(x._1)))).filter(x => f.contains(x._3)).map(x => {
      val beta_B: Set[X] = f(x._3).filter(y => y.isLeft).map(y => y.left.get).flatMap(y => x._4._1(y)).toSet
      val gamma_B: Int = f(x._3).filter(y => y.isLeft).map(y => y.left.get).map(y => x._4._2(y)).reduce(_ + _)
      ((x._1, beta_B), x._2, gamma_B + f(x._3).filter(y => y.isRight).size, q_bottom)
    }).toSet

    def getStatesAndDelta1(queue: List[(Q, Set[X])],
                           res_states: Set[(Q, Set[X])],
                           res_delta: Set[((Q, Set[X]), Σ, Int, (Q, Set[X]))],
                           delta: Set[(Q, Σ, Q, (Map[X, Set[X]], Map[X, Int]))]
                          ): (Set[(Q, Set[X])], Set[((Q, Set[X]), Σ, Int, (Q, Set[X]))]) = {
      queue match {
        case s :: rest =>
          val newRules = delta.filter(r => r._3 == s._1).map(r => (((r._1, s._2.flatMap(x => r._4._1(x))), r._2, s._2.foldLeft(0) { (x, y) => x + r._4._2(y) }, s)))
          getStatesAndDelta1(rest ++ newRules.map(x => x._1).diff(res_states), res_states ++ newRules.map(x => x._1), res_delta ++ newRules, delta)
        case Nil => (res_states, res_delta)
      }
    }

    val (states, delta1) = getStatesAndDelta1(delta2.map(x => x._1).toList, delta2.map(x => x._1), Set[((Q, Set[X]), Σ, Int, (Q, Set[X]))](), δ.map(x => (x._1._1, x._1._2, x._2, h(η(x._1)))).toSet)

    implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
      def append(f1: Int, f2: => Int): Int = f1 + f2

      def zero: Int = 0
    }

    nondeterministic.Transducer(
      states.map(x => Left(x): Either[(Q, Set[X]), Int]) + Right(q_bottom),
      states.filter(x => x._1 == s0).map(x => Left(x): Either[(Q, Set[X]), Int]),
      delta1.map(r => (Left(r._1): Either[(Q, Set[X]), Int], r._2, Left(r._4), r._3)) ++ delta2.map(r => (Left(r._1), r._2, Right(r._4), r._3)),
      Set[Either[(Q, Set[X]), Int]](Right(q_bottom))
    )
  }

  def toParikhImage: Set[(Map[Γ, Int], Set[Map[Γ, Int]])] = {
    val alphabets: Set[Γ] = η.toList.flatMap(x => x._2.toList).flatMap(x => x._2).filter(x => x.isRight).map(x => x.right.get).toSet ++
      f.flatMap(x => x._2).filter(x => x.isRight).map(x => x.right.get).toSet
    val aToI: Map[Γ, Int] = (alphabets.toList.indices zip alphabets).toMap.map(x => x._2 -> x._1)

    implicit def intMonoid: Monoid[Map[Int, Int]] = new Monoid[Map[Int, Int]] {
      def append(f1: Map[Int, Int], f2: => Map[Int, Int]): Map[Int, Int] = alphabets.map(c => aToI(c) -> (f1.withDefaultValue(0)(aToI(c)) + f2.withDefaultValue(0)(aToI(c)))).toMap

      def zero: Map[Int, Int] = alphabets.map(c => aToI(c) -> 0).toMap
    }

    val trans = toMapTransducer
    val trans1: nondeterministic.Transducer[Either[(Q, Set[X]), Int], Σ, Map[Int, Int]] = nondeterministic.Transducer(
      trans.states,
      trans.initStates,
      trans.δ.map(r => (r._1, r._2, r._3, r._4.map(x => aToI(x._1) -> x._2))),
      trans.F
    )

    val r: Set[(Map[Γ, Int], Set[Map[Γ, Int]])] = MapRegExp.eval(MapRegExp.toRegExp(trans1)) match {
      case m: MapRegExp.CharExp =>
        m.c.map(x => {
          (alphabets.map(c => c -> x._1.withDefaultValue(0)(aToI(c))).toMap,
            x._2.filterNot(y => y.isEmpty).map(y => alphabets.map(c => c -> y.withDefaultValue(0)(aToI(c))).toMap))
        })
      case MapRegExp.EmptyExp => Set()
    }

    if (f.contains(s0)) r ++ Set[(Map[Γ, Int], Set[Map[Γ, Int]])]((
      alphabets.map(c => c -> f(s0).filter(x => x.isRight).map(x => x.right.get).groupBy(identity).mapValues(_.size).withDefaultValue(0)(c)).toMap
      , Set.empty))
    else r
  }

  def toMapTransducer: nondeterministic.Transducer[Either[(Q, Set[X]), Int], Σ, Map[Γ, Int]] = {

    val alphabets: Set[Γ] = η.toList.flatMap(x => x._2.toList).flatMap(x => x._2).filter(x => x.isRight).map(x => x.right.get).toSet ++
      f.flatMap(x => x._2).filter(x => x.isRight).map(x => x.right.get).toSet

    def h(alpha: Map[X, List[Either[X, Γ]]]): (Map[X, Set[X]], Map[X, Map[Γ, Int]]) =
      (alpha.mapValues(_.filter(x => x.isLeft).map(x => x.left.get).toSet),
        alpha.mapValues(_.filter(x => x.isRight).map(x => x.right.get).groupBy(identity).mapValues(_.size)))

    val q_bottom: Int = 1

    val delta2: Set[((Q, Set[X]), Σ, Map[Γ, Int], Int)] = δ.map(x => (x._1._1, x._1._2, x._2, h(η(x._1)))).filter(x => f.contains(x._3)).map(x => {
      val beta_B: Set[X] = f(x._3).filter(y => y.isLeft).map(y => y.left.get).flatMap(y => x._4._1(y)).toSet
      val gamma_B: Map[Γ, Int] = alphabets.map(c => {
        val len1: Int = f(x._3).filter(y => y.isRight).map(y => y.right.get).groupBy(identity).mapValues(_.size).withDefaultValue(0)(c)
        val len2: Int = f(x._3).filter(y => y.isLeft).map(y => y.left.get).map(y => x._4._2(y).withDefaultValue(0)(c)).reduce(_ + _)
        c -> (len1 + len2)
      }).toMap

      ((x._1, beta_B), x._2, gamma_B, q_bottom)
    }).toSet

    def getStatesAndDelta1(queue: List[(Q, Set[X])],
                           res_states: Set[(Q, Set[X])],
                           res_delta: Set[((Q, Set[X]), Σ, Map[Γ, Int], (Q, Set[X]))],
                           delta: Set[(Q, Σ, Q, (Map[X, Set[X]], Map[X, Map[Γ, Int]]))]
                          ): (Set[(Q, Set[X])], Set[((Q, Set[X]), Σ, Map[Γ, Int], (Q, Set[X]))]) = {
      queue match {
        case s :: rest =>
          val newRules = delta.filter(r => r._3 == s._1).map(r => {
            val q1: Q = s._1
            val B: Set[X] = s._2
            val q0: Q = r._1
            val beta = r._4._1
            val gamma = r._4._2
            val beta_B: Set[X] = B.flatMap(y => beta(y))
            val gamma_B: Map[Γ, Int] = alphabets.map(c => {
              c -> B.foldLeft(0) { (p, q) => p + gamma(q).withDefaultValue(0)(c) }
            }).toMap
            ((q0, beta_B), r._2, gamma_B, (q1, B))
          })

          getStatesAndDelta1(rest ++ newRules.map(x => x._1).diff(res_states), res_states ++ newRules.map(x => x._1), res_delta ++ newRules, delta)
        case Nil => (res_states, res_delta)
      }
    }

    val (states, delta1) = getStatesAndDelta1(delta2.map(x => x._1).toList, delta2.map(x => x._1), Set[((Q, Set[X]), Σ, Map[Γ, Int], (Q, Set[X]))](), δ.map(x => (x._1._1, x._1._2, x._2, h(η(x._1)))).toSet)

    implicit def intMonoid: Monoid[Map[Γ, Int]] = new Monoid[Map[Γ, Int]] {
      def append(f1: Map[Γ, Int], f2: => Map[Γ, Int]): Map[Γ, Int] = alphabets.map(c => c -> (f1.withDefaultValue(0)(c) + f2.withDefaultValue(0)(c))).toMap

      def zero: Map[Γ, Int] = alphabets.map(c => c -> 0).toMap
    }

    nondeterministic.Transducer(
      states.map(x => Left(x): Either[(Q, Set[X]), Int]) + Right(q_bottom),
      states.filter(x => x._1 == s0).map(x => Left(x): Either[(Q, Set[X]), Int]),
      delta1.map(r => (Left(r._1): Either[(Q, Set[X]), Int], r._2, Left(r._4), r._3)) ++ delta2.map(r => (Left(r._1), r._2, Right(r._4), r._3)),
      Set[Either[(Q, Set[X]), Int]](Right(q_bottom))
    )
  }
}