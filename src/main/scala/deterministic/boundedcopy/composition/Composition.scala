package deterministic.boundedcopy.composition

import java.util.Calendar

import deterministic.DFA
import deterministic.boundedcopy.SST

case class Composition(printOption: Boolean) {

  def compose[Q1, Q2, A, B, C, X, Y](sst1: SST[Q1, A, B, X], sst2: SST[Q2, B, C, Y]) = {
    val boundness = calcBoundedness(sst2)
    val t1 = System.currentTimeMillis()
    if (printOption) {
      println("Start Composition of SSTs: ")
      println("    " + sst1)
      println("    " + sst2)
      println()
      println("    Start Composition to Monoid SST: ")
      println("        " + getTime())
    }
    val msst0 = composeToMonoidSST(sst1, sst2)

    if (printOption) {
      println("    End Composition to Monoid SST: ")
      println("        " + getTime())
      println("        " + msst0.sst)
      println()
      println("    Start Trim Vars of Monoid SST: ")
      println("        " + getTime())
    }
    val msst2 = msst0.trimVars

    if (printOption) {
      println("    End Trim Vars of Monoid SST: ")
      println("        " + getTime())
      println("        " + msst2.sst)
      println()
      println("    Start Conversion to SST: ")
      println("        " + getTime())
    }
    val res0 = convertFromMonoidSST(boundness, msst2).rename("r0")

    if (printOption) {
      println("    End Conversion to SST: ")
      println("        " + getTime())
      println("        " + res0)
      println()
      println("    Start Trim Vars of SST: ")
      println("        " + getTime())
    }

    val res = res0.trimVars
    if (printOption) {
      val t2 = System.currentTimeMillis()
      println("    End Trim Vars of SST: ")
      println("        " + getTime())
      println("        " + res)
      println("End Composition SST. The result is:")
      println("    " + res)
      println("    time : " + (t2 - t1) / 1000.0 + "s")
      println()
    }
    res
  }

  /**
    * first step of composition.
    */
  def composeToMonoidSST[Q1, Q2, A, B, C, X, Y](sst1: SST[Q1, A, B, X], sst2: SST[Q2, B, C, Y]) = {
    type Trans = Map[(Q2, X), Q2]
    type Update1 = Map[X, List[Either[X, B]]]
    type Update2 = Map[Y, List[Either[Y, C]]]

    def innerDelta(f: Trans)(q2: Q2, xb: Either[X, B]): Option[Q2] = {
      xb match {
        case Left(x) => f.get(q2, x)
        case Right(b) => sst2.δ.get(q2, b)
      }
    }

    def innerDeltaHat(f: Trans)(q2: Q2, xbs: List[Either[X, B]]): Option[Q2] = {
      xbs match {
        case (xb :: xbs) => {
          val q_next = innerDelta(f)(q2, xb)
          if (q_next.isEmpty)
            None
          else
            innerDeltaHat(f)(q_next.get, xbs)
        }
        case Nil => Some(q2)
      }
    }

    def innerEta(q2: Q2, xb: Either[X, B]): Option[List[Either[(Q2, X), Update2]]] = {
      xb match {
        case Left(x) => Some(List(Left((q2, x))))
        case Right(b) if sst2.η.contains((q2, b)) => Some(List(Right(sst2.η(q2, b))))
        case _ => Some(List())
      }
    }

    def innerEtaHat(f: Trans)(q2: Q2, xbs: List[Either[X, B]]): Option[List[Either[(Q2, X), Update2]]] = {

      def _innerEtaHat(f: Trans, q2: Q2, xbs: List[Either[X, B]], res: List[Either[(Q2, X), Update2]]): Option[List[Either[(Q2, X), Update2]]] = {
        xbs match {
          case (xb :: xbs) => {
            val q_next = innerDelta(f)(q2, xb)
            if (q_next.isEmpty)
              None
            else
              _innerEtaHat(f, q_next.get, xbs, res ::: innerEta(q2, xb).get)
          }
          case _ => Some(res)
        }
      }

      _innerEtaHat(f, q2, xbs, List())
    }

    def largeDelta(f: Trans, m: Update1): Trans = {
      sst2.states.flatMap(q => sst1.vars.map(x => (q, x) -> innerDeltaHat(f)(q, m(x)))).collect {
        case r if r._2.nonEmpty => r._1 -> r._2.get
      }.toMap
    }

    def largeEta(f: Trans, m: Update1): Map[(Q2, X), List[Either[(Q2, X), Update2]]] = {
      sst2.states.flatMap(q => sst1.vars.map(x => (q, x) -> innerEtaHat(f)(q, m(x)))).collect {
        case r if r._2.nonEmpty => r._1 -> r._2.get
      }.toMap
    }

    def eta(q1: Q1, f: Trans, a: A) = largeEta(f, sst1.η(q1, a))

    def final1(q1: Q1, f: Trans) = sst1.f.get(q1).map(innerEtaHat(f)(sst2.s0, _)).collect { case Some(t) => t }

    def final2(q1: Q1, f: Trans) = sst1.f.get(q1).map(u => innerDeltaHat(f)(sst2.s0, u)).collect { case Some(t) => t }
      .flatMap(sst2.f.get(_))

    if (printOption) println("            Find s0     : " + getTime())
    val initial = (sst1.s0, (for (q2 <- sst2.states; x <- sst1.vars) yield ((q2, x), q2)).toMap)

    def searchStatesAndDelta: (Set[(Q1, Trans)], Map[((Q1, Trans), A), (Q1, Trans)]) = {
      def rec(queue: List[(Q1, Trans)], statesSet: Set[(Q1, Trans)], transitionMap: Map[((Q1, Trans), A), (Q1, Trans)]): (Set[(Q1, Trans)], Map[((Q1, Trans), A), (Q1, Trans)]) = {
        queue match {
          case (q :: qs) => {
            val transitions = sst1.δ.filter(r => r._1._1 == q._1).map(r => (q, r._1._2) -> (r._2, largeDelta(q._2, sst1.η(q._1, r._1._2))))
            val states0 = transitions.toSet
            val states = states0.map(r => r._2).filterNot(statesSet(_))
            rec(qs ::: states.toList, statesSet ++ states, transitionMap ++ transitions)
          }
          case _ => (statesSet, transitionMap)
        }
      }

      rec(List(initial), Set(initial), Map())
    }

    if (printOption) println("            Find states and delta: " + getTime())
    val (states, deltaMap) = searchStatesAndDelta

    if (printOption) println("                Q     : " + states.size)
    if (printOption) println("                Delta : " + deltaMap.size)
    if (printOption) println("            Find vars   : " + getTime())
    val vars = for (q2 <- sst2.states; x <- sst1.vars) yield (q2, x)

    if (printOption) println("            Find f1     : " + getTime())
    val f1 = (for ((q, f) <- states; u <- final1(q, f)) yield ((q, f), u)).toMap

    if (printOption) println("            Find f2     : " + getTime())
    val f2 = (for ((q, f) <- states; v <- final2(q, f)) yield ((q, f), v)).toMap

    if (printOption) println("            Trim states : " + getTime())
    val sMap = states.zipWithIndex.toMap
    val dMap = sMap.map(t => t._2 -> t._1)
    val dfa0: DFA[Int, A] = DFA(sMap.map(_._2).toSet, sMap(initial), deltaMap.map(t => (sMap(t._1._1), t._1._2) -> sMap(t._2)), f1.map(t => sMap(t._1)).toSet)
    val dfa = dfa0.trim

    if (printOption) println("                Q     : " + dfa.states.size)
    if (printOption) println("                Delta : " + dfa.δ.size)

    if (printOption) println("            Find eta    : " + getTime())
    val etaMap = dfa.δ.map(t => t._1 -> eta(dMap(t._1._1)._1, dMap(t._1._1)._2, t._1._2))

    val sst: SST[Int, A, Update2, (Q2, X)] = SST(dfa.states, dfa.s0, vars, dfa.δ, etaMap, dfa.f.map(q => q -> f1(dMap(q))).toMap)

    MonoidSST(sst, sst2.vars, f2.map(t => sMap(t._1) -> t._2))
  }

  def getVariables[X, A](u: List[Either[X, A]]): List[X] = {
    u.collect { case Left(x) => x }
  }

  def getAlphabet[X, A](u: List[Either[X, A]]): List[A] = {
    u.collect { case Right(x) => x }
  }

  /**
    * \pi_1 in thesis
    */
  def resolveShuffle[X, A](m: Map[X, List[Either[X, A]]]): Map[X, List[X]] = {
    m.mapValues(xas => getVariables(xas))
  }

  def getAlphabetsUntilVariable[X, A](xas: List[Either[X, A]]): List[A] = getAlphabet(xas.takeWhile(_.isRight))

  def dropLeadingAlphabets[X, A](xas: List[Either[X, A]]): List[Either[X, A]] = xas.dropWhile(_.isRight)

  def findNthString[X, A](xas: List[Either[X, A]], x0: X, k0: Int): List[A] = {
    xas match {
      case (Left(x) :: xas) if x == x0 && k0 <= 0 => getAlphabetsUntilVariable(xas)
      case (Left(x) :: xas) if x == x0 => findNthString(xas, x0, k0 - 1)
      case (Left(x) :: xas) => findNthString(xas, x0, k0)
      case (Right(x) :: xas) => findNthString(xas, x0, k0)
      case _ => List()
    }
  }

  /**
    * \pi_2 in thesis
    */
  def resolveStore[X, A](vars: List[X], m: Map[X, List[Either[X, A]]])(x0: X, k0: Int): List[A] = {
    if (k0 == 0) {
      getAlphabetsUntilVariable(m(x0))
    } else {
      findNthString(vars.flatMap(x => dropLeadingAlphabets(m(x))), x0, k0 - 1)
    }
  }

  def numberNthOccurence[X, A](vars: List[X], s: Map[X, List[X]]): Map[X, List[Either[X, (X, Int)]]] = {
    type Counter = Map[X, Int]

    def inc(counter: Counter, x: X): Counter = counter.updated(x, counter(x) + 1)

    def column(counter: Counter, xs: List[X]): (List[Either[X, (X, Int)]], Counter) = {
      xs match {
        case (x :: xs) => {
          val (result, counterResult) = column(inc(counter, x), xs)
          (Left(x) :: Right((x, counter(x))) :: result, counterResult)
        }
        case _ => (List(), counter)
      }
    }

    def row(counter: Counter, ys: List[X]): Map[X, List[Either[X, (X, Int)]]] = {
      ys match {
        case (y :: ys) => {
          val (result, counterRow) = column(counter, s(y))
          row(counterRow, ys) + (y -> (Right((y, 0)) :: result))
        }
        case _ => Map()
      }
    }

    row(Map().withDefaultValue(1), vars)
  }

  /**
    * \pi{-1} in thesis
    */
  def synthesize[X, A](vars: List[X], s: Map[X, List[X]], a: (X, Int) => List[A]): Map[X, List[Either[X, A]]] = {
    numberNthOccurence(vars, s).mapValues(row =>
      row.flatMap(xk => xk match {
        case Left(x) => List(Left(x))
        case Right((x, k)) => a(x, k).map(Right(_))
      })
    )
  }

  /**
    * there is no alphabet in definition of SST, so guess it from transition function
    */
  def guessAlphabet[A](sst: SST[_, A, _, _]): Set[A] = sst.δ.keySet.map(_._2)

  /**
    * second step of composition
    */
  def convertFromMonoidSST[Q, A, B, X, Y](boundedness: Int, msst: MonoidSST[Q, A, B, X, Y]): SST[Int, A, B, (X, Y, Int)] = {
    type Shuffle = Map[Y, List[Y]]
    type UpdateM = Map[X, List[Either[X, Map[Y, List[Either[Y, B]]]]]]
    type Bone = Map[X, Shuffle]
    type Var = (X, Y, Int)

    val vars2: List[Y] = msst.vars2.toList

    def iota(b: Bone)(x: X): Map[Y, List[Either[Y, Either[Var, B]]]] = {
      synthesize(vars2, b(x), (y, k) => List(Left((x, y, k))))
    }

    def duplicateRight(u: List[Either[Y, B]]): List[Either[Y, Either[Var, B]]] = {
      u.map(yb => yb match {
        case Left(y) => Left(y)
        case Right(b) => Right(Right(b))
      })
    }

    def toRightUpdate(m: Map[Y, List[Either[Y, B]]]): Map[Y, List[Either[Y, Either[Var, B]]]] = {
      m.mapValues(row => duplicateRight(row))
    }

    def iotaHom(bone: Bone)(xms: List[Either[X, Map[Y, List[Either[Y, B]]]]]):
    Map[Y, List[Either[Y, Either[Var, B]]]] = {
      xms match {
        case (Left(x) :: xms) => Update.composite(msst.vars2, iota(bone)(x), iotaHom(bone)(xms))
        case (Right(m) :: xms) => Update.composite(msst.vars2, toRightUpdate(m), iotaHom(bone)(xms))
        case _ => Update.identity(msst.vars2)
      }
    }

    def largeDeltaPrime(bone: Bone, m: UpdateM): Bone = {
      m.map(t => t._1 -> resolveShuffle(iotaHom(bone)(t._2)))
    }

    def largeEtaPrime(bone: Bone, m: UpdateM) = {
      val t = for (x <- msst.sst.vars; y <- msst.vars2; k <- 0 to boundedness) yield (x, y, k)
      t.collect {
        case (x, y, k) if m.contains(x) => ((x, y, k), resolveStore(vars2, iotaHom(bone)(m(x)))(y, k))
      }.toMap
    }

    def eta(q: Q, b: Bone, a: A) = largeEtaPrime(b, msst.sst.η(q, a))

    def final0(q: Q, b: Bone) = {
      msst.sst.f.get(q).flatMap(u => {
        msst.final2.get(q).map(v => {
          val m = iotaHom(b)(u)
          getAlphabet(Update.hatHom(m, duplicateRight(v)))
        })
      })
    }

    if (printOption) println("            Find s0     : " + getTime())
    val initial = (msst.sst.s0, (for (x <- msst.sst.vars) yield (x, Update.identityShuffle(msst.vars2))).toMap)

    def searchStatesAndDelta: (Set[(Q, Bone)], Map[((Q, Bone), A), (Q, Bone)]) = {
      def rec(queue: List[(Q, Bone)], statesSet: Set[(Q, Bone)], transitionMap: Map[((Q, Bone), A), (Q, Bone)]): (Set[(Q, Bone)], Map[((Q, Bone), A), (Q, Bone)]) = {
        queue match {
          case (q :: qs) => {
            val transitions = msst.sst.δ.filter(r => r._1._1 == q._1).map(r => (q, r._1._2) -> (r._2, largeDeltaPrime(q._2, msst.sst.η(q._1, r._1._2))))
            val states0 = transitions.toSet
            val states = states0.map(r => r._2).filterNot(statesSet(_))
            rec(qs ::: states.toList, statesSet ++ states, transitionMap ++ transitions)
          }
          case _ => (statesSet, transitionMap)
        }
      }

      rec(List(initial), Set(initial), Map())
    }

    if (printOption) println("            Find states and delta : " + getTime())
    val (states, deltaMap) = searchStatesAndDelta
    if (printOption) println("                Q     : " + states.size)
    if (printOption) println("                Delta : " + deltaMap.size)

    if (printOption) println("            Find vars   : " + getTime())
    val vars = for (x <- msst.sst.vars; y <- msst.vars2; k <- 0 to boundedness) yield (x, y, k)

    if (printOption) println("            Find f      : " + getTime())
    val f = (for ((q, f) <- states; u <- final0(q, f)) yield ((q, f), u)).toMap

    if (printOption) println("            Find delta  : " + getTime())

    if (printOption) println("            Trim states : " + getTime())
    val sMap = states.zipWithIndex.toMap
    val dMap = sMap.map(t => t._2 -> t._1)
    val dfa0: DFA[Int, A] = DFA(sMap.map(_._2).toSet, sMap(initial), deltaMap.map(t => (sMap(t._1._1), t._1._2) -> sMap(t._2)), f.map(t => sMap(t._1)).toSet)
    val dfa = dfa0.trim
    if (printOption) println("                Q     : " + dfa.states.size)
    if (printOption) println("                Delta : " + dfa.δ.size)

    if (printOption) println("            Find eta    : " + getTime())
    val etaMap = dfa.δ.map(t => t._1 -> eta(dMap(t._1._1)._1, dMap(t._1._1)._2, t._1._2))

    SST(dfa.states, dfa.s0, vars, dfa.δ, etaMap, dfa.f.map(q => q -> f(dMap(q))).toMap)
  }

  /**
    * calcualate boundedness of give SST
    *
    * @note this procedure may NOT halt if copyful SST was given
    */
  def calcBoundedness[Q, A, B, X](sst: SST[Q, A, B, X]): Int = {
    // ((x, y) -> n) means var x used y for n times
    if (sst.vars.isEmpty)
      return 0

    val one = (for (x <- sst.vars; y <- sst.vars) yield ((x, y), if (x == y) {
      1
    } else {
      0
    })).toMap
    val initials = (for (q <- sst.states) yield (q, one)).toList

    def trans(qm: (Q, Map[(X, X), Int]), a: A): (Q, Map[(X, X), Int]) = {
      val (q0, m0) = qm
      (sst.δ(q0, a), (for ((x, v) <- sst.η(q0, a); y <- sst.vars) yield ((x, y), v.collect { case Left(z) => m0(z, y) }.sum)))
    }

    def searchStates: Set[(Q, Map[(X, X), Int])] = {
      def rec(queue: List[(Q, Map[(X, X), Int])], openSet: Set[(Q, Map[(X, X), Int])]): Set[(Q, Map[(X, X), Int])] = {
        queue match {
          case (q :: qs) => {
            val next = sst.δ.filter(r => r._1._1 == q._1).toList.map(r => (trans(q, r._1._2))).filterNot(openSet(_))
            rec(qs ::: next, openSet ++ next)
          }
          case _ => openSet
        }
      }

      rec(initials, initials.toSet)
    }

    val reachables = searchStates
    reachables.toSeq.map { case (_, m) => sst.vars.toSeq.map(y => sst.vars.toSeq.map(x => m(x, y)).sum).max }.max
  }

  def getTime(): String = {
    val now = Calendar.getInstance().getTime()
    now.toString.split(" ")(3)
  }
}
