package regex

object IntRegExp extends RegExpHelper[Set[((Int, Int), Set[(Int, Int)])], Int] {
  override def eval(r: IntRegExp.RegExp): RegExp = {
    r match {
      case EmptyExp => EmptyExp
      case c: CharExp => c
      case c: ConcatExp => {
        (eval(c.r1), eval(c.r2)) match {
          case (m1: CharExp, m2: CharExp) => CharExp(m1.c.flatMap(x => m2.c.map(y => ((x._1._1 + y._1._1, x._1._2 + y._1._2), x._2 ++ y._2))))
          case _ => EmptyExp
        }
      }
      case c: AltExp => {
        (eval(c.r1), eval(c.r2)) match {
          case (EmptyExp, x) => x
          case (x, EmptyExp) => x
          case (m1: CharExp, m2: CharExp) => CharExp(m1.c ++ m2.c)
          case _ => EmptyExp
        }
      }
      case c: StarExp => {
        eval(c.r) match {
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
  }

  override def h(a: Int) = Set(((1, a), Set.empty))

}
