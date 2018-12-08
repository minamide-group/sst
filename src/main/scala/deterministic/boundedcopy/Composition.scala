package deterministic.boundedcopy

object Update {
  def composite[X, Γ](vars: Set[X], m1: Map[X, List[Either[X, Γ]]], m2: Map[X, List[Either[X, Γ]]]): Map[X, List[Either[X, Γ]]] = {
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
}

case class MonoidSST[Q, A, B, X, Y](
  sst: SST[Q, A, Map[Y, List[Either[Y, B]]], X],
  final2: Map[Q, List[Either[Y, B]]]) {
}

object Composition {
  def search[Q, A](initialState: Q, alphabet: Set[A], transition: (Q, A) => Q): Set[Q] = {
    def rec(queue: List[Q], openSet: Set[Q]): Set[Q] = {
      queue match {
        case (q :: qs) => {
          val next: Set[Q] = alphabet.map(a => transition(q, a)).filterNot(q2 => openSet.contains(q2))
          rec(next.toList ++ queue, openSet union next)
        }
        case _ => Set()
      }
    }
    rec(List(initialState), Set())
  }

  def composeToMonoidSST[Q1, Q2, A, B, C, X, Y](sst1: SST[Q1, A, B, X], sst2: SST[Q2, B, C, Y]): 
        MonoidSST[(Q1, Map[(Q2, X), Q2]), A, C, (Q2, X), Y] = {
    type Trans = Map[(Q2, X), Q2]
    type Update1 = Map[X, List[Either[X, B]]]
    type Update2 = Map[Y, List[Either[Y, C]]]

    def innerDelta(f: Trans)(q2: Q2, xb: Either[X, B]): Q2 = {
      xb match {
        case Left(x)  => f(q2, x)
        case Right(b) => sst2.δ((q2, b))
      }
    }

    def innerDeltaHat(f: Trans)(q2: Q2, xbs: List[Either[X, B]]): Q2 = {
      xbs match {
        case (xb :: xbs) => innerDeltaHat(f)(innerDelta(f)(q2, xb), xbs)
        case _ => q2
      }
    }

    def innerEta(q2: Q2, xb: Either[X, B]): List[Either[(Q2, X), Update2]] = {
      xb match {
        case Left(x)  => List(Left((q2, x)))
        case Right(b) => List(Right(sst2.η(q2, b)))
      }
    }

    def innerEtaHat(f: Trans)(q2: Q2, xbs: List[Either[X, B]]): List[Either[(Q2, X), Update2]] = {
      xbs match {
        case (xb :: xbs)  => innerEta(q2, xb) ++ innerEtaHat(f)(innerDelta(f)(q2, xb), xbs)
        case _ => List()
      }
    }

    def largeDelta(f: Trans, m: Update1): Trans = {
      (for (q2 <- sst2.states; x <- sst1.vars)
        yield ((q2, x), innerDeltaHat(f)(q2, m(x)))
      ).toMap
    }

    def largeEta(f: Trans, m: Update1): Map[(Q2, X), List[Either[(Q2, X), Update2]]] = {
      (for (q2 <- sst2.states; x <- sst1.vars)
        yield ((q2, x), innerEtaHat(f)(q2, m(x)))
      ).toMap
    }

    def delta(q1: Q1, f: Trans, a: A): (Q1, Trans) = (sst1.δ(q1, a), largeDelta(f, sst1.η(q1, a)))
    def eta(q1: Q1, f: Trans, a: A) = largeEta(f, sst1.η(q1, a))
    def final1(q1: Q1, f: Trans) = sst1.f.get(q1).map(innerEtaHat(f)(sst2.s0, _))
    def final2(q1: Q1, f: Trans) = sst1.f.get(q1).flatMap(u => sst2.f.get(innerDeltaHat(f)(sst2.s0, u)))

    // there is no set of alphabet, we guess it...
    val alphabet: Set[A] = sst1.δ.keySet.map(_._2)
    val initial = (sst1.s0, (for (q2 <- sst2.states;  x <- sst1.vars) yield ((q2, x), q2)).toMap)
    val states = search(initial, alphabet, (qf: (Q1, Trans), a: A) => delta(qf._1, qf._2, a))
    val vars = for (q2 <- sst2.states; x <- sst1.vars) yield (q2, x)
    val deltaMap = (for ((q, f) <- states; a <- alphabet) yield (((q, f), a), delta(q, f, a))).toMap
    val etaMap = (for ((q, f) <- states; a <- alphabet) yield (((q, f), a), eta(q, f, a))).toMap
    val f1 = (for ((q, f) <- states; u <- final1(q, f)) yield ((q, f), u)).toMap
    val f2 = (for ((q, f) <- states; v <- final2(q, f)) yield ((q, f), v)).toMap



    val sst = SST(states, initial, vars, deltaMap, etaMap, f1)
    MonoidSST(sst, f2)
  }
}