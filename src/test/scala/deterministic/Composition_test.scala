package deterministic.boundedcopy

import org.scalatest._


class Composition_test extends FlatSpec {
  val rev = {
    val q0 = 0
    val x0 = 'x'

    val delta = Map(
      (q0,'a')->q0,
      (q0,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Right('a'), Left(x0))
      ),
      (q0,'b')->Map(
        x0->List(Right('b'),Left(x0))
      )
    )

    val f = Map(q0->List(Left(x0)))
    SST(Set(q0), q0, Set(x0), delta, eta, f)
  }

  val half1 = {
    val q0 = 0
    val q1 = 1
    val x0 = 'x'

    val delta = Map(
      (q0,'a')->q1,
      (q0,'b')->q1,
      (q1,'a')->q0,
      (q1,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x0))
      ),
      (q0,'b')->Map(
        x0->List(Left(x0))
      ),
      (q1,'a')->Map(
        x0->List(Left(x0), Right('a'))
      ),
      (q1,'b')->Map(
        x0->List(Left(x0), Right('b'))
      )
    )

    val f = Map(q0->List(Left(x0)), q1->List(Left(x0)))
    SST(Set(q0, q1), q0, Set(x0), delta, eta, f)
  }

  val half2 = {
    val q0 = 0
    val x0 = 'x'
    val x1 = 'y'

    val delta = Map(
      (q0,'a')->q0,
      (q0,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x1), Right('a')),
        x1->List(Left(x0))
      ),
      (q0,'b')->Map(
        x0->List(Left(x1), Right('b')),
        x1->List(Left(x0))
      ),
    )

    val f = Map(q0->List(Left(x1)))
    SST(Set(q0), q0, Set(x0, x1), delta, eta, f)
  }

  val dup = {
    val q0 = 0
    val x0 = 'x'
    val x1 = 'y'

    val delta = Map(
      (q0,'a')->q0,
      (q0,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x0), Right('a')),
        x1->List(Left(x1), Right('a'))
      ),
      (q0,'b')->Map(
        x0->List(Left(x0), Right('b')),
        x1->List(Left(x1), Right('b'))
      ),
    )

    val f = Map(q0->List(Left(x0), Left(x1)))
    SST(Set(q0), q0, Set(x0, x1), delta, eta, f)
  }

  val f1 = {
    val q0 = 0
    val q1 = 1
    val a = Right('a')
    val b = Right('b')
    val x = Left('x')
    val y = Left('y')
    val z = Left('z')
    
    val delta = Map(
      (q0,'a')->q1,
      (q0,'b')->q0,
      (q1,'a')->q1,
      (q1,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        'x'->List(x, a),
        'y'->List(a, y, a),
        'z'->List(x)
      ),
      (q0,'b')->Map(
        'x'->List(x, b),
        'y'->List(),
        'z'->List()
      ),
      (q1,'a')->Map(
        'x'->List(x, a),
        'y'->List(a, y, a),
        'z'->List(z)
      ),
      (q1,'b')->Map(
        'x'->List(z, y, b),
        'y'->List(),
        'z'->List()
      ),
    )

    val f = Map(q1->List(x))
    SST(Set(q0, q1), q0, Set('x', 'y', 'z'), delta, eta, f)
  }


  def getRandomString(length: Int, chars: List[Char]): Seq[Char] = {
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (_ <- 0 to length-1)
      sb.append(chars(r.nextInt(chars.size)))
    sb.toSeq
  }

  def inspectSST[Q, A, B, X](sst: SST[Q, A, B, X]) {
    println(sst.states.size, "states: ", sst.states)
    println(sst.vars.size, "vars:   ", sst.vars)
    println(sst.δ.size, "delta:  ", sst.δ)
    println(sst.η.size, "eta:    ", sst.η)
    println(sst.f.size, "F:      ", sst.f)
  }

  def inspectMonoidSST[Q, A, B, X, Y](msst: MonoidSST[Q, A, B, X, Y]) {
    inspectSST(msst.sst)
    println("vars2:   ", msst.vars2)
    println("F2:      ", msst.final2)
  }

  def resultToOption[Q, A](result: (Boolean, Q, Seq[A])): Option[Seq[A]] = {
    if (result._1) {
      Some(result._3)
    } else {
      None
    }
  }

  def testComposition[Q, R, X, Y](sst1: SST[Q, Char, Char, X], sst2: SST[R, Char, Char, Y]) {
    val comp = Composition.compose(sst1, sst2)
    val r = new scala.util.Random

    for (_ <- 0 to 100) {
      val input = getRandomString(r.nextInt(100), List('a', 'b'))
      val result = resultToOption(comp.process(input))
      val expected = resultToOption(sst1.process(input)) flatMap (x => resultToOption(sst2.process(x)))

      assert(result.mkString == expected.mkString)
    }
  }

  "composition" should "output the same" in {
    val list = List(rev, half1, half2, dup, f1)
    for (sst1 <- list; sst2 <- list) {
      testComposition(sst1, sst2)
    }
  }

  "calcBoundedness" should "stop with correct result" in {
    assert(Composition.calcBoundedness(rev) == 1)
    assert(Composition.calcBoundedness(half1) == 1)
    assert(Composition.calcBoundedness(half2) == 1)
    assert(Composition.calcBoundedness(dup) == 1)
    assert(Composition.calcBoundedness(f1) == 2)
  }
}
