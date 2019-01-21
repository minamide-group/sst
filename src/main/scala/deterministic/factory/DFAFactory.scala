package deterministic.factory

import expression.regex._
import nondeterministic.NFA

case class DFAFactory() {

  class S

  trait C

  case object Eps extends C

  case class Ch(c: Char) extends C

  private def unitNFA(c: Char): NFA[S, C] = {
    val q0 = new S
    val q1 = new S
    NFA(Set(q0, q1), q0, Map((q0, Ch(c)) -> Set(q1)), Set(q1))
  }

  private def emptyNFA(): NFA[S, C] = {
    val q0 = new S
    NFA(Set(q0), q0, Map[(S, C), Set[S]](), Set())
  }

  private def epsilonNFA(): NFA[S, C] = {
    val q0 = new S
    val q1 = new S
    NFA(Set(q0, q1), q0, Map((q0, Eps) -> Set(q1)), Set(q1))
  }

  private def altNFA(r1: NFA[S, C], r2: NFA[S, C]): NFA[S, C] = {
    val q0 = new S
    val q1 = new S
    NFA(
      r1.states ++ r2.states + q0 + q1,
      q0,
      r1.δ ++ r2.δ ++ Map((q0, Eps) -> Set(r1.s0, r2.s0)) ++
        r1.f.map(q => (q, Eps) -> (r1.δ.withDefaultValue(Set())(q, Eps) + q1)).toMap ++
        r2.f.map(q => (q, Eps) -> (r2.δ.withDefaultValue(Set())(q, Eps) + q1)).toMap,
      Set(q1)
    )
  }

  private def conNFA(r1: NFA[S, C], r2: NFA[S, C]): NFA[S, C] = {
    val q0 = new S
    val q1 = new S
    NFA(
      r1.states ++ r2.states + q0 + q1,
      q0,
      r1.δ ++ r2.δ ++
        r1.f.map(q => (q, Eps) -> (r1.δ.withDefaultValue(Set())(q, Eps) + r2.s0)).toMap ++
        r2.f.map(q => (q, Eps) -> (r2.δ.withDefaultValue(Set())(q, Eps) + q1)).toMap ++
        Map((q0, Eps) -> Set(r1.s0)),
      Set(q1)
    )
  }

  private def starNFA(r1: NFA[S, C]): NFA[S, C] = {
    val q0 = new S
    val q1 = new S
    NFA(
      r1.states + q0 + q1,
      q0,
      r1.δ ++ r1.f.map(q => (q, Eps) -> (r1.δ.withDefaultValue(Set())(q, Eps) + q1)) ++ Map(
        (q0, Eps) -> Set(r1.s0, q1),
        (q1, Eps) -> Set(q0)
      ),
      Set(q1)
    )
  }

  def getDFA(regex: String) = removeEps(RegToNFA(parseReg(regex.filterNot(_.isWhitespace)))).toDFA.trim.minimize.rename

  def parseReg(str: String): RegExp = {
    //assert valid

    def f(str: String, start: Int, endChars: Set[Char]): (RegExp, Int) = {
      var cur: RegExp = EpsExp
      var idx = start
      while (idx < str.length && !endChars(str.charAt(idx))) {
        str.charAt(idx) match {
          case '|' => {
            val (rex, nextIdx) = f(str, idx + 1, Set(')', '|'))
            cur = AltExp(cur, rex)
            idx = nextIdx
          }
          case '(' => {
            val (rex, nextIdx) = f(str, idx + 1, Set(')'))
            if ((nextIdx + 1) < str.length && str.charAt(nextIdx + 1) == '*') {
              cur = ConcatExp(cur, StarExp(rex))
              idx = nextIdx + 2
            }
            else {
              cur = ConcatExp(cur, rex)
              idx = nextIdx + 1
            }
          }
          case c if (idx + 1 < str.length && str.charAt(idx + 1) == '*') => {
            cur = ConcatExp(cur, StarExp(CharExp(c)))
            idx = idx + 2
          }
          case c => {
            cur = ConcatExp(cur, CharExp(c))
            idx = idx + 1
          }
        }
      }

      (cur, idx)
    }

    f(str, 0, Set())._1
  }

  def RegToNFA(regex: RegExp): NFA[S, C] = {
    regex match {
      case ce: CharExp[Char] => unitNFA(ce.c)
      case ae: AltExp => altNFA(RegToNFA(ae.r1), RegToNFA(ae.r2))
      case con: ConcatExp => conNFA(RegToNFA(con.r1), RegToNFA(con.r2))
      case se: StarExp => starNFA(RegToNFA(se.r))
      case EmptyExp => emptyNFA()
      case EpsExp => epsilonNFA()
    }
  }

  def removeEps[Q](nfa: NFA[Q, C]): NFA[Q, Char] = {

    def star(s: Set[Q], next: Map[Q, Set[Q]]): Set[Q] = {
      val newS = s.flatMap(q => next.withDefaultValue(Set())(q))
      if (s ++ newS == s) s
      else star(s ++ newS, next)
    }

    val next = nfa.δ.filter(r => r._1._2 == Eps).map(r => r._1._1 -> r._2)

    val rules = nfa.δ.filterNot(r => r._1._2 == Eps)

    val closure = next.map(r => r._1 -> star(Set(r._1), next))

    val newDelta = closure.flatMap(t =>
      rules.filter(r => t._2(r._1._1)).groupBy(_._1._2).map(d => (t._1, d._1) -> d._2.flatMap(_._2).toSet)
    )

    val delta: Map[(Q, Char), Set[Q]] = (rules ++ newDelta).map(t =>
      t._1._2 match {
        case Ch(c) => (t._1._1, c) -> t._2
      }
    )

    val newF = nfa.f ++ closure.filterNot(t => t._2.intersect(nfa.f).isEmpty).map(_._1)

    NFA(nfa.states, nfa.s0, delta, newF)
  }

}
