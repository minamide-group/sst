package deterministic

object RegExpHelper {

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

  def toRegExp[Q, Σ](transducer : nondeterministic.Transducer[Q, Σ, Int]): RegExp ={
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

    val (states, initialStates, finialStates, delta) = (transducer.states, transducer.initStates, transducer.F, transducer.δ)

    initialStates.flatMap(q0 => {
      finialStates.map(qf => {
        val rules = eliminate(states.filterNot(x => x == q0).filterNot(x => x == qf).toList,
          delta.map(x => (x._1, CharExp(Set(((1, x._4), Set.empty))): RegExp, x._3)))
        val A = getCombined(q0, q0, rules)
        val B = getCombined(q0, qf, rules)
        val C = getCombined(qf, q0, rules)
        val D = getCombined(qf, qf, rules)
        ConcatExp(ConcatExp(StarExp(AltExp(A, ConcatExp(ConcatExp(B, StarExp(D)), C))), B), StarExp(D))
      })
    }).foldLeft(EmptyExp: RegExp) { (x, y) => AltExp(x, y) }
  }
}
