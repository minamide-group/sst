package deterministic.boundedcopy

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


  /**
    * @param input : input sequence
    * @param q     : start state
    * @return output function X=> (X∪Γ)*
    */
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

  def toRegExp: RegExp = {
    def getCombined[Q](q1: Q, q2: Q, rules: Set[(Q, RegExp, Q)]): RegExp =
      rules.filter(x => x._1 == q1).filter(x => x._3 == q2).map(x => x._2).foldLeft(EmptyExp: RegExp) { (x, y) => AltExp(x, y) }

    def eliminate[Q](states: List[Q], rules: Set[(Q, RegExp, Q)]): Set[(Q, RegExp, Q)] = {
      states match {
        case s :: rest =>
          val newRules: Set[(Q, RegExp, Q)] = rules.filter(x => x._3 == s).flatMap(r1 => {
            rules.filter(x => x._1 == s).map(r2 => (r1._1, AltExp(getCombined(r1._1, r2._3, rules), ConcatExp(ConcatExp(r1._2, StarExp(getCombined(s, s, rules))), r2._2)), r2._3))
          })
          eliminate(rest, newRules.union(rules.filterNot(x => x._1 == s).filterNot(x => x._3 == s).filterNot(x => newRules.map(r => (r._1, r._3)).contains(x._1, x._3))))
        case Nil => rules
      }
    }

    val (states, initialStates, finialStates, delta) = toIntegerTransducer

    val regex: RegExp = initialStates.flatMap(q0 => {
      finialStates.map(qf => {
        val rules = eliminate(states.filterNot(x => x == q0).filterNot(x => x == qf).toList,
          delta.map(x => (x._1, CharExp(Set(((1, x._3), Set.empty))): RegExp, x._4)))
        val A = getCombined(q0, q0, rules)
        val B = getCombined(q0, qf, rules)
        val C = getCombined(qf, q0, rules)
        val D = getCombined(qf, qf, rules)
        ConcatExp(ConcatExp(StarExp(AltExp(A, ConcatExp(ConcatExp(B, StarExp(D)), C))), B), StarExp(D))
      })
    }).foldLeft(EmptyExp: RegExp) { (x, y) => AltExp(x, y) }

    if (f.contains(s0)) AltExp(regex, CharExp(Set(((0, f(s0).filter(x => x.isRight).size), Set.empty)))) else regex
  }

  /**
    * @return (Q, Q_0, F, delta1, delta2)
    */
  def toIntegerTransducer: (Set[Either[(Q, Map[X, Int]), Int]], //Q
    Set[Either[(Q, Map[X, Int]), Int]], //Q0
    Set[Either[(Q, Map[X, Int]), Int]], //F
    Set[(Either[(Q, Map[X, Int]), Int], Σ, Int, Either[(Q, Map[X, Int]), Int])], //delta
    ) = {

    def h(alpha: Map[X, List[Either[X, Γ]]]): (Map[X, Map[X, Int]], Map[X, Int]) =
      (alpha.map(x => x._1 -> x._2.filter(y => y.isLeft).map(y => y.left.get).groupBy(identity).mapValues(_.size)),
        alpha.mapValues(_.count(x => x.isRight)))

    val q_bottom: Int = 0

    val delta2: Set[((Q, Map[X, Int]), Σ, Int, Int)] = δ.map(x => (x._1._1, x._1._2, x._2, h(η(x._1)))).filter(x => f.contains(x._3)).map(x => {
      val beta = x._4._1
      val gamma = x._4._2
      val Bf: Map[X, Int] = f(x._3).filter(x => x.isLeft).map(x => x.left.get).groupBy(identity).mapValues(_.size)
      val len1: Int = f(x._3).count(x => x.isRight)
      val len2: Int = Bf.map(x => x._1 -> x._2 * gamma.withDefaultValue(0)(x._1)).foldLeft(0) { (p, q) => p + q._2 }
      val B: Map[X, Int] = vars.map(x => {
        x -> Bf.map(y => y._1 -> y._2 * beta.withDefaultValue(Map())(y._1).withDefaultValue(0)(x)).foldLeft(0) { (p, q) => p + q._2 }
      }).toMap

      ((x._1, B), x._2, len1 + len2, q_bottom)
    }).toSet

    def getStatesAndDelta1(queue: List[(Q, Map[X, Int])],
                           res_states: Set[(Q, Map[X, Int])],
                           res_delta: Set[((Q, Map[X, Int]), Σ, Int, (Q, Map[X, Int]))],
                           delta: Set[(Q, Σ, Q, (Map[X, Map[X, Int]], Map[X, Int]))]
                          ): (Set[(Q, Map[X, Int])], Set[((Q, Map[X, Int]), Σ, Int, (Q, Map[X, Int]))]) = {
      queue match {
        case s :: rest =>
          val q1: Q = s._1
          val B: Map[X, Int] = s._2
          val newRules: Set[((Q, Map[X, Int]), Σ, Int, (Q, Map[X, Int]))] = delta.filter(x => x._3 == q1).map(rule => {
            val beta = rule._4._1
            val gamma = rule._4._2
            val q0 = rule._1
            val len = B.map(y => y._1 -> y._2 * gamma.withDefaultValue(0)(y._1)).foldLeft(0) { (p, q) => p + q._2 }
            val BB = vars.map(x => {
              x -> B.map(y => y._1 -> y._2 * beta.withDefaultValue(Map())(y._1).withDefaultValue(0)(x)).foldLeft(0) { (p, q) => p + q._2 }
            }).toMap
            ((q0, BB), rule._2, len, (q1, B))
          })
          getStatesAndDelta1(rest ++ newRules.map(x => x._1).diff(res_states), res_states ++ newRules.map(x => x._1), res_delta ++ newRules, delta)
        case Nil => (res_states, res_delta)
      }
    }

    val (states, delta1) = getStatesAndDelta1(delta2.map(x => x._1).toList, delta2.map(x => x._1), Set[((Q, Map[X, Int]), Σ, Int, (Q, Map[X, Int]))](), δ.map(x => (x._1._1, x._1._2, x._2, h(η(x._1)))).toSet)

    (states.map(x => Left(x): Either[(Q, Map[X, Int]), Int]) + Right(q_bottom),
      states.filter(x => x._1 == s0).map(x => Left(x): Either[(Q, Map[X, Int]), Int]),
      Set(Right(q_bottom)),
      delta1.map(r => (Left(r._1), r._2, r._3, Left(r._4))) ++ delta2.map(r => (Left(r._1), r._2, r._3, Right(r._4))))
  }

  trait RegExp {
    def eval: RegExp
  }

  case class CharExp(c: Set[((Int, Int), Set[(Int, Int)])]) extends RegExp {
    def eval: RegExp = this
  }

  case class ConcatExp(r1: RegExp, r2: RegExp) extends RegExp {
    def eval: RegExp = {
      (r1.eval, r2.eval) match {
        case (EmptyExp, _) => EmptyExp
        case (_, EmptyExp) => EmptyExp
        case (m1: CharExp, m2: CharExp) => CharExp(m1.c.flatMap(x => m2.c.map(y => ((x._1._1 + y._1._1, x._1._2 + y._1._2), x._2 ++ y._2))))
      }
    }
  }

  case class AltExp(r1: RegExp, r2: RegExp) extends RegExp {
    def eval: RegExp = {
      (r1.eval, r2.eval) match {
        case (EmptyExp, x) => x
        case (x, EmptyExp) => x
        case (m1: CharExp, m2: CharExp) => CharExp(m1.c ++ m2.c)
      }
    }
  }

  case class StarExp(r: RegExp) extends RegExp {
    def eval: RegExp = {
      r.eval match {
        case EmptyExp => CharExp(Set(((0, 0), Set.empty)))
        case m: CharExp => {
          def _star(list: List[((Int, Int), Set[(Int, Int)])], set: Set[((Int, Int), Set[(Int, Int)])]): Set[((Int, Int), Set[(Int, Int)])] = {
            list match {
              case x :: rest if x._2.isEmpty => _star(rest, set)
              case x :: rest => _star(rest, set ++ set.map(y => ((x._1._1 + y._1._1, x._1._2 + y._1._2), x._2 ++ y._2)))
              case Nil => set
            }
          }

          CharExp(_star(m.c.toList, Set(((0, 0), m.c.map(x => x._1)))))
        }
      }
    }
  }

  case object EmptyExp extends RegExp {
    def eval: RegExp = this
  }

}