package nondeterministic

case class NFA[Q, Σ](
                      states: Set[Q],
                      s0: Q,
                      σ: Map[(Q, Σ), Set[Q]],
                      f: Set[Q]) {

  def process(input: Seq[Σ]): (Boolean, Set[Q]) = {
    val finalStates = trans(input)(Set(s0)).intersect(f)
    (!finalStates.isEmpty, finalStates)
  }

  def trans(input: Seq[Σ])(cur: Set[Q]): Set[Q] = {
    input match {
      case Seq(c, cs@_*) => trans(cs)(cur.flatMap(x => σ(x, c)))
      case _ => cur
    }
  }

  def toRegEx: Option[String] = {
    def concat(a: Option[String], b: Option[String]): Option[String] = {
      (a, b) match {
        case (Some(s1), Some(s2)) => Some(s1 + s2)
        case _ => None
      }
    }

    def star(c: Option[String]): Option[String] = {
      c match {
        case Some(s) => Some("(" + s + ")*")
        case None => Some("")
      }
    }

    def union(a: Option[String], b: Option[String]): Option[String] = {
      (a, b) match {
        case (None, _) => b
        case (_, None) => a
        case _ => Some(a.get + "|" + b.get)
      }
    }

    def getCombined[Q](q1: Q, q2: Q, rules: Set[(Q, Option[String], Q)]): Option[String] =
      rules.filter(x => x._1 == q1).filter(x => x._3 == q2).map(x => x._2).foldLeft(None: Option[String]) { (x, y) => union(x, y) }

    def eliminate(states: List[Q], rules: Set[(Q, Option[String], Q)]): Set[(Q, Option[String], Q)] = {
      states match {
        case s :: rest =>
          val newRules = rules.filter(x => x._3 == s).flatMap(r1 => {
            rules.filter(x => x._1 == s).map(r2 => (r1._1, union(getCombined(r1._1, r2._3, rules), concat(concat(r1._2, star(getCombined(s, s, rules))), r2._2)), r2._3))
          })
          eliminate(rest, newRules.union(rules.filterNot(x => x._1 == s).filterNot(x => x._3 == s).filterNot(x => newRules.map(r => (r._1, r._3)).contains(x._1, x._3))))
        case Nil => rules
      }
    }

    f.map(q_f => {
      val rules: Set[(Q, Option[String], Q)] = eliminate(states.filterNot(x => x == s0).filterNot(x => x == q_f).toList, σ.toSet.flatMap((r: ((Q, Σ), Set[Q])) => r._2.map(q1 => (r._1._1, Some(r._1._2.toString), q1))))

      s0 match {
        case q if q == q_f =>
          rules.filter(x => x._1 == s0).filter(x => x._3 == s0).map(x => x._2).foldLeft(Some("ε"): Option[String]) { (x, y) => union(x, y) }
        case _ =>
          val A = getCombined(s0, s0, rules)
          val B = getCombined(s0, q_f, rules)
          val C = getCombined(q_f, s0, rules)
          val D = getCombined(q_f, q_f, rules)
          concat(concat(star(union(A, concat(concat(B, star(D)), C))), B), star(D))
      }
    }).foldLeft(None: Option[String]) { (x, y) => union(x, y) }
  }
}
