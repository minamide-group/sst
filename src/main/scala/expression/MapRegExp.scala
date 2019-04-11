package expression

import expression.regex._

object MapRegExp extends AbstractRegExp[Set[(Map[Int, Int], Set[Map[Int, Int]])], Map[Int, Int]] {
  type CharExp1 = CharExp[Set[(Map[Int, Int], Set[Map[Int, Int]])]]

  override def eval(r : RegExp): RegExp = {
    r match {
      case EmptyExp => EmptyExp
      case EpsExp => CharExp(Set[(Map[Int, Int], Set[Map[Int, Int]])]((Map().withDefaultValue(0), Set.empty)))
      case c: CharExp1 => c
      case c: ConcatExp => {
        (eval(c.r1), eval(c.r2)) match {
          case (m1: CharExp1, m2: CharExp1) =>
            val charSet: Set[Int] = m1.c.flatMap(x => x._1).map(x => x._1) ++ m2.c.flatMap(x => x._1).map(x => x._1)
            CharExp(m1.c.flatMap(x => m2.c.map(y => (charSet.map(c => c -> (x._1.withDefaultValue(0)(c) + y._1.withDefaultValue(0)(c))).toMap, x._2 ++ y._2))))
          case _ => EmptyExp
        }
      }
      case c: AltExp => {
        (eval(c.r1), eval(c.r2)) match {
          case (EmptyExp, x) => x
          case (x, EmptyExp) => x
          case (m1: CharExp1, m2: CharExp1) => CharExp(m1.c ++ m2.c)
          case _ => EmptyExp
        }
      }
      case c: StarExp => {
        eval(c.r) match {
          case EmptyExp => eval(EpsExp)
          case m: CharExp1 => {
            def _star(list: List[(Map[Int, Int], Set[Map[Int, Int]])], set: Set[(Map[Int, Int], Set[Map[Int, Int]])]): Set[(Map[Int, Int], Set[Map[Int, Int]])] = {
              list match {
                case x :: rest if x._2.isEmpty => _star(rest, set)
                case x :: rest => {
                  val newSet = set.map(y => {
                    val charSet: Set[Int] = x._1.keys.toSet ++ y._1.keys.toSet
                    (charSet.map(c => c -> (x._1.withDefaultValue(0)(c) + y._1.withDefaultValue(0)(c))).toMap, (x._2 ++ y._2))
                  })
                  _star(rest, set ++ newSet)
                }
                case Nil => set
              }
            }

            CharExp(_star(m.c.toList, Set((Map().withDefaultValue(0), m.c.map(x => x._1)))))
          }
        }
      }
    }
  }

  override def h(a: Map[Int, Int]): Set[(Map[Int, Int], Set[Map[Int, Int]])] = Set((a, Set.empty))

}
