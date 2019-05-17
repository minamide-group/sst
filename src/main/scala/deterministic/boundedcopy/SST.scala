package deterministic.boundedcopy

import constraint.vars.{SST_State, SST_Var}
import deterministic.DFA
import expression._
import expression.regex._
import scalaz.Monoid

case class SST[Q, Σ, Γ, X](
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

    if (f.contains(result._1)) {
      (true, result._1, eval(f(result._1), result._2))
    } else {
      (false, result._1, Seq())
    }
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

    _trans(input)(q)(vars.map(x => (x, List(Left(x)))).toMap)
  }

  def trim: SST[Q, Σ, Γ, X] = trimStates.trimVars

  def trimStates: SST[Q, Σ, Γ, X] = {
    val res0 = toDFA.trim
    SST(res0.states, res0.s0, vars, res0.δ, η.filter(r => res0.δ.contains(r._1)), f.filter(r => res0.states(r._1)))
  }

  def toDFA = DFA(states, s0, δ, f.keySet)

  def trimVars: SST[Q, Σ, Γ, X] = {
    val newVars = usedVars.intersect(nonEmptyVars)

    val newEta = η.map(
      r => r._1 -> r._2.filter(x => newVars(x._1)).map(x => x._1 -> x._2.filter(e => (e.isRight) || (e.isLeft && newVars(e.left.get))))
    ).withDefaultValue(newVars.map(x => x -> List()).toMap.withDefaultValue(List()))

    val newF = f.map(t => t._1 -> t._2.filter(e => (e.isRight) || (e.isLeft && newVars(e.left.get))))

    SST(states, s0, newVars, δ, newEta, newF)
  }

  def usedVars: Set[X] = {
    val simpleDelta : Map[(Q, Q), Map[X, Set[X]]] = δ.map(r => ((r._1._1, r._2), r._1._2, η.withDefaultValue(Map())(r._1))).groupBy(_._1).map(t=> {
      def getNext(queue : List[Map[X, Set[X]]], res : Map[X, Set[X]]):Map[X, Set[X]] = {
        queue match {
          case Nil => res
          case m :: rest => getNext(rest, vars.map(x=> x-> (m.withDefaultValue(Set())(x) ++ res.withDefaultValue(Set())(x)) ).toMap)
        }
      }

      t._1 -> getNext(t._2.map(_._3).toList.map(_.map(tt=> tt._1-> tt._2.collect{case Left(x)=>x}.toSet)), Map())
    })

    def search(queue: List[(Q, Set[X])], qToXs: Map[Q, Set[X]]): Map[Q, Set[X]] = {
      queue match {
        case Nil => qToXs
        case ss :: rest => {
          val newS: Map[Q, Set[X]] = simpleDelta.filter(r => r._1._2 == ss._1).map(r =>
            (r._1._1, ss._2.flatMap(x => r._2(x) ++ ss._2 ))
          ).filterNot(t => qToXs.contains(t._1) && t._2.subsetOf(qToXs(t._1)))

          search(rest ::: newS.toList, states.map(q => q -> (newS.withDefaultValue(Set())(q) ++ qToXs.withDefaultValue(Set())(q))).toMap)
        }
      }
    }

    val queue: List[(Q, Set[X])] = f.map(t => (t._1, t._2.collect { case Left(x) => x }.toSet)).toList
    search(queue, queue.toMap).withDefaultValue(Set[X]())(s0)
  }

  def nonEmptyVars: Set[X] = {
    val simpleDelta : Map[(Q, Q), Map[X, (Set[X], Set[Γ])]] = δ.map(r => ((r._1._1, r._2), r._1._2, η.withDefaultValue(Map())(r._1))).groupBy(_._1).map(t=> {
      def getNext(queue : List[Map[X, (Set[X], Set[Γ])]], res : Map[X, (Set[X], Set[Γ])]):Map[X, (Set[X], Set[Γ])] = {
        queue match {
          case Nil => res
          case m :: rest => getNext(rest, vars.map(x=>
            x->(m.withDefaultValue((Set(), Set()))(x)._1 ++ res.withDefaultValue((Set(), Set()))(x)._1,
                m.withDefaultValue((Set(), Set()))(x)._2 ++ res.withDefaultValue((Set(), Set()))(x)._2)
            ).toMap)
        }
      }

      t._1 -> getNext(t._2.map(_._3).toList.map(_.map(tt=>
        tt._1-> (tt._2.collect{case Left(x)=>x}.toSet, tt._2.collect{case Right(c)=>c}.toSet))), Map())
    })

    def search(queue: List[(Q, Set[X])], qToXs: Map[Q, Set[X]]): Map[Q, Set[X]] = {
      queue match {
        case Nil => qToXs
        case ss :: rest => {
          val newS: Map[Q, Set[X]] = simpleDelta.filter(r => r._1._1 == ss._1).map(r => {
            (r._1._2, r._2.filter(t => t._2._2.nonEmpty || t._2._1.intersect(ss._2).nonEmpty).map(_._1).toSet ++ ss._2)
          }).filterNot(t => qToXs.contains(t._1) && t._2.subsetOf(qToXs(t._1)))

          search(rest ::: newS.toList, states.map(q => q -> (newS.withDefaultValue(Set())(q) ++ qToXs.withDefaultValue(Set())(q))).toMap)
        }
      }
    }

    val queue: List[(Q, Set[X])] = List((s0, Set()))
    search(queue, queue.toMap).withDefaultValue(Set()).filter(t => f.contains(t._1)).foldLeft(Set[X]()) { (x, y) => x ++ y._2 }
  }

  def rename(sstName: String): SST[SST_State, Σ, Γ, SST_Var] = {

    val toNewState: Map[Q, SST_State] = (states + s0).toList.zipWithIndex.map(x => x._1 -> SST_State(x._2, sstName)).toMap

    val toNewVar: Map[X, SST_Var] = vars.toList.zipWithIndex.map(x => x._1 -> SST_Var(x._2, sstName)).toMap

    val newStates: Set[SST_State] = states.map(s => toNewState(s))

    val newS0: SST_State = toNewState.withDefaultValue(SST_State(0, sstName))(s0)

    val newVars: Set[SST_Var] = vars.map(x => toNewVar(x))

    val newDelta: Map[(SST_State, Σ), SST_State] = δ.map(r => (toNewState(r._1._1), r._1._2) -> toNewState(r._2))

    val newEta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Γ]]]] = η.map(r =>
      (toNewState(r._1._1), r._1._2) -> r._2.map(
        t => toNewVar(t._1) -> t._2.map(
          e => if (e.isLeft) Left(toNewVar(e.left.get)) else Right(e.right.get)
        )
      )
    )

    val newF: Map[SST_State, List[Either[SST_Var, Γ]]] = f.map(r =>
      toNewState(r._1) -> r._2.map(
        e => if (e.isLeft) Left(toNewVar(e.left.get)) else Right(e.right.get)
      )
    )

    SST(newStates, newS0, newVars, newDelta, newEta, newF)
  }

  def print {
    println("--------start----------")
    println("states num: " + states.size)
    println("----------")
    println("vars num: " + vars.size)
    println("----------")
    println("delta size: " + δ.size)
    println("----------")
    println("eta size: " + η.size)
    println("---------end-----------")
  }

  def printDetail {
    def listA[A, B](list: List[Either[A, B]]): List[Any] = list.collect {
      case Left(a) => a
      case Right(b) => b
    }

    println("--------start----------")
    println("states: ")
    states.foreach(println)
    println("----------")
    println("init state: " + s0)
    println("----------")
    println("vars: ")
    vars.foreach(println)
    println("----------")
    println("output function:")
    f.map(t => t._1 -> listA(t._2)).foreach(println)
    println("----------")
    println("delta:")
    δ.foreach(println)
    println("----------")
    println("eta:")
    η.map(r => r._1 -> r._2.map(t => t._1 -> listA(t._2))).foreach(println)
    println("---------end-----------")

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
    val trans1: nondeterministic.Transducer[Either[(Q, Map[X, Int]), Int], Σ, Map[Int, Int]] = nondeterministic.Transducer(
      trans.states,
      trans.s0,
      trans.δ.map(r => (r._1, r._2, r._3, r._4.map(x => aToI(x._1) -> x._2))),
      trans.f
    )

    val r: Set[(Map[Γ, Int], Set[Map[Γ, Int]])] = MapRegExp.eval(MapRegExp.toRegExp(trans1)) match {
      case m: MapRegExp.CharExp1 =>
        m.c.map(x => {
          (alphabets.map(c => c -> x._1.withDefaultValue(0)(aToI(c))).toMap,
            x._2.filterNot(m => m.isEmpty).map(y => alphabets.map(c => c -> y.withDefaultValue(0)(aToI(c))).toMap))
        })
      case EmptyExp => Set()
    }

    if (f.contains(s0)) r ++ Set[(Map[Γ, Int], Set[Map[Γ, Int]])]((
      alphabets.map(c => c -> f(s0).filter(x => x.isRight).map(x => x.right.get).groupBy(identity).mapValues(_.size).withDefaultValue(0)(c)).toMap
      , Set.empty)) else r
  }

  def toMapTransducer: nondeterministic.Transducer[Either[(Q, Map[X, Int]), Int], Σ, Map[Γ, Int]] = {

    val alphabets: Set[Γ] = η.toList.flatMap(x => x._2.toList).flatMap(x => x._2).filter(x => x.isRight).map(x => x.right.get).toSet ++
      f.flatMap(x => x._2).filter(x => x.isRight).map(x => x.right.get).toSet

    def h(alpha: Map[X, List[Either[X, Γ]]]): (Map[X, Map[X, Int]], Map[X, Map[Γ, Int]]) =
      (alpha.mapValues(_.filter(y => y.isLeft).map(y => y.left.get).groupBy(identity).mapValues(_.size)),
        alpha.mapValues(_.filter(x => x.isRight).map(x => x.right.get).groupBy(identity).mapValues(_.size)))

    val q_bottom: Int = 3

    val delta2: Set[((Q, Map[X, Int]), Σ, Map[Γ, Int], Int)] = δ.map(x => (x._1._1, x._1._2, x._2, h(η(x._1)))).filter(x => f.contains(x._3)).map(x => {
      val beta = x._4._1
      val gamma = x._4._2
      val Bf: Map[X, Int] = f(x._3).filter(x => x.isLeft).map(x => x.left.get).groupBy(identity).mapValues(_.size)
      val beta_B: Map[X, Int] = vars.map(x => {
        x -> Bf.map(y => y._1 -> y._2 * beta.withDefaultValue(Map())(y._1).withDefaultValue(0)(x)).foldLeft(0) { (p, q) => p + q._2 }
      }).toMap
      val gamma_B: Map[Γ, Int] = alphabets.map(c => {
        val len1: Int = f(x._3).filter(y => y.isRight).map(y => y.right.get).groupBy(identity).mapValues(_.size).withDefaultValue(0)(c)
        val len2: Int = f(x._3).filter(y => y.isLeft).map(y => y.left.get).map(y => gamma(y).withDefaultValue(0)(c) * Bf.withDefaultValue(0)(y)).reduce(_ + _)
        c -> (len1 + len2)
      }).toMap

      ((x._1, beta_B), x._2, gamma_B, q_bottom)
    }).toSet

    def getStatesAndDelta1(queue: List[(Q, Map[X, Int])],
                           res_states: Set[(Q, Map[X, Int])],
                           res_delta: Set[((Q, Map[X, Int]), Σ, Map[Γ, Int], (Q, Map[X, Int]))],
                           delta: Set[(Q, Σ, Q, (Map[X, Map[X, Int]], Map[X, Map[Γ, Int]]))]
                          ): (Set[(Q, Map[X, Int])], Set[((Q, Map[X, Int]), Σ, Map[Γ, Int], (Q, Map[X, Int]))]) = {
      queue match {
        case s :: rest =>
          val newRules = delta.filter(r => r._3 == s._1).map(r => {
            val q1: Q = s._1
            val B: Map[X, Int] = s._2
            val q0: Q = r._1
            val beta = r._4._1
            val gamma = r._4._2
            val beta_B = vars.map(x => {
              x -> B.map(y => y._1 -> y._2 * beta.withDefaultValue(Map())(y._1).withDefaultValue(0)(x)).foldLeft(0) { (p, q) => p + q._2 }
            }).toMap
            val gamma_B: Map[Γ, Int] = alphabets.map(c => {
              c -> B.map(y => y._2 * gamma.withDefaultValue(Map())(y._1).withDefaultValue(0)(c)).reduce(_ + _)
            }).toMap

            ((q0, beta_B), r._2, gamma_B, (q1, B))
          })

          getStatesAndDelta1(rest ++ newRules.map(x => x._1).diff(res_states), res_states ++ newRules.map(x => x._1), res_delta ++ newRules, delta)
        case Nil => (res_states, res_delta)
      }
    }

    val (states, delta1) = getStatesAndDelta1(delta2.map(x => x._1).toList, delta2.map(x => x._1),
      Set[((Q, Map[X, Int]), Σ, Map[Γ, Int], (Q, Map[X, Int]))](),
      δ.map(x => (x._1._1, x._1._2, x._2, h(η(x._1)))).toSet)

    implicit def intMonoid: Monoid[Map[Γ, Int]] = new Monoid[Map[Γ, Int]] {
      def append(f1: Map[Γ, Int], f2: => Map[Γ, Int]): Map[Γ, Int] = alphabets.map(c => c -> (f1.withDefaultValue(0)(c) + f2.withDefaultValue(0)(c))).toMap

      def zero: Map[Γ, Int] = alphabets.map(c => c -> 0).toMap
    }

    nondeterministic.Transducer(
      states.map(x => Left(x): Either[(Q, Map[X, Int]), Int]) + Right(q_bottom),
      states.filter(x => x._1 == s0).map(x => Left(x): Either[(Q, Map[X, Int]), Int]),
      delta1.map(r => (Left(r._1): Either[(Q, Map[X, Int]), Int], r._2, Left(r._4), r._3)) ++ delta2.map(r => (Left(r._1), r._2, Right(r._4), r._3)),
      Set[Either[(Q, Map[X, Int]), Int]](Right(q_bottom))
    )
  }

}
